import * as M from '@taquito/michel-codec';

export class GenerateApiError implements Error {
    public name = `GenerateApiError`;
    constructor(public message: string, public readonly data: unknown) {
        console.error(`❌ GenerateApiError: ${message}`, data);
    }
}
const assertExhaustive = (value: never, message: string) => {
    console.error(message, { value });
};

export const generateContractApi = (contractScript: string): {
    schema: {
        methods: {
            [name: string]: {
                params: SchemaType;
            };
        };
    };
    typescriptCode: {
        final: string;
        methods: string;
    };
} => {

    const p = new M.Parser();

    const contract = p.parseScript(contractScript) as M.MichelsonContract;
    if (!contract) {
        throw new GenerateApiError(`Could not parse contract script`, contractScript);
    }

    const contractStorage = contract.find(x => x.prim === `storage`) as undefined | M.MichelsonContractStorage;
    const contractParameter = contract.find(x => x.prim === `parameter`) as undefined | M.MichelsonContractParameter;

    const storageResult = contractStorage && visitContractStorage(contractStorage);
    const storage = storageResult ?? { storage: { raw: { prim: `never` } as M.MichelsonType, fields: [] } };

    const parameterResult = contractParameter && visitContractParameter(contractParameter);
    const methods = parameterResult?.methods ?? [];
    const { schemaMethods } = toSchema(methods);

    const typescriptCode = toTypescriptCode(storage, methods);

    // console.log(`methods`);
    // console.log(methods);
    // console.log(schemaMethods);
    // console.log(typescriptCode.methods);

    return {
        schema: {
            methods: schemaMethods,
        },
        typescriptCode,
        // schema: {
        //     methods: methodObj,
        //     storage: JSON.parse(JSON.stringify(schemaStorageExtr)),
        // },
        // singatures: {
        //     methods: methodSignatures,
        // }
    };
};

const toTypescriptCode = (storage: TypedStorage, methods: TypedMethod[]): {
    final: string;
    methods: string;
} => {

    // Not really tabs :)
    const tabs = (indent: number) => Array(indent).fill(`    `).join(``);
    const toIndentedItems = (indent: number, delimeter: string, items: string[]) => {
        return `
${tabs(indent + 1)}${items.join(`${delimeter}
${tabs(indent + 1)}`)}
${tabs(indent)}`;
    };

    const typeToCode = (t: TypedType, indent: number): string => {
        if (t.typescriptType) {
            //return `${t.typescriptType}`;
            // Strict mode
            if (t.typescriptType === `boolean`) {
                return `${t.typescriptType}`;
            }

            return `${t.typescriptType} & { __type: '${`prim` in t.raw ? t.raw.prim : `unknown`}' }`;
        }
        if (t.array) {
            return `${typeToCode(t.array.item, indent)}[]`;
        }
        if (t.map) {
            //             return `{
            // ${tabs(indent + 1)}[key: ${typeToCode(t.map.key, indent)}]: ${typeToCode(t.map.value, indent + 1)};
            // ${tabs(indent)}}`;
            return `Map<${typeToCode(t.map.key, indent)}, ${typeToCode(t.map.value, indent)}>`;
        }
        if (t.fields) {
            return `{${toIndentedItems(indent, ``,
                t.fields.map((a, i) => varToCode(a, i, indent + 1) + `;`),
            )}}`;
        }
        if (t.unit) {
            return `void`;
        }
        if (t.never) {
            return `never`;
        }
        if (t.value) {
            return `${t.value}`;
        }

        throw new GenerateApiError(`Unknown type node`, { t });
    };

    const varToCode = (t: TypedVar, i: number, indent: number): string => {
        return `${t.name ?? i}${t.type.optional ? `?` : ``}: ${typeToCode(t.type, indent)}`;
    };

    const argsToCode = (args: TypedVar[], indent: number): string => {
        if (args.length === 1) {
            if (args[0].type.unit) { return ``; }
            return `${args[0].name ?? `param`}: ${typeToCode(args[0].type, indent + 1)}`;
        }

        return `params: {${toIndentedItems(indent, ``,
            args.filter(x => x.name || !x.type.unit).map((a, i) => varToCode(a, i, indent + 1) + `;`),
        )}}`;
    };

    const methodsToCode = (indent: number) => {
        const methodFields = methods.map(x => {
            const methodCode = `${x.name}: (${argsToCode(x.args, indent + 1)}) => Promise<void>;`;
            return methodCode;
        });

        const methodsTypeCode = `type Methods = {${toIndentedItems(indent, ``, methodFields)}};`;
        return methodsTypeCode;
    };

    const storageToCode = (indent: number) => {
        const storageTypeCode = `type Storage = ${typeToCode(storage.storage, indent)};`;
        return storageTypeCode;
    };

    const methodsCode = methodsToCode(0);
    const storageCode = storageToCode(0);

    const finalCode = `
${storageCode}

${methodsCode}

export type Contract = { methods: Methods, storage: Storage };
`;
    return {
        final: finalCode,
        methods: methodsCode,
    };

};

type SchemaObjectType = { [name: string]: SchemaType };
type SchemaType = string | SchemaType[] | SchemaObjectType;

const toSchema = (methods: TypedMethod[]) => {

    const getSchemaObjectType = (vars: TypedVar[]) => {
        // console.log('getSchemaObjectType', { vars });

        if (vars.some(x => !x)) {
            throw new GenerateApiError(`getSchemaObjectType has null vars`, { vars });
        }

        return vars.reduce((out, x, i) => {
            out[x.name ?? i] = getSchemaType(x.type);
            return out;
        }, {} as SchemaObjectType);
    };

    const getSchemaType = (t: TypedType): SchemaType => {
        // console.log('getSchemaType', { t });

        return t.value
            ?? (t.array ? [getSchemaType(t.array.item)] : null)
            ?? (t.map ? [`map`, getSchemaType(t.map.key), getSchemaType(t.map.value)] : null)
            ?? (t.fields ? getSchemaObjectType(t.fields) : null)
            ?? (t.unit ? `unit` : null)
            ?? (t.never ? `never` : null)
            ?? `${t.raw as unknown as string}`;
    };

    const schemaMethods = methods.reduce((out, x) => {
        // console.log('schemaMethods', { x });

        out[x.name] = {
            params: x.args.length === 1 && !x.args[0].name ? getSchemaType(x.args[0].type) : getSchemaObjectType(x.args ?? []),
        };
        return out;
    }, {} as {
        [name: string]: {
            params: SchemaType;
        };
    });

    return {
        schemaMethods,
    };
};

type TypedStorage = {
    storage: {
        raw: M.MichelsonType;
        fields: TypedVar[];
    };
};
type TypedParameter = {
    methods: TypedMethod[];
};
type TypedMethod = {
    name: string;
    args: TypedVar[];
};
type TypedVar = {
    name?: string;
    type: TypedType;
};
type TypedType = {
    raw: M.MichelsonType;
    unit?: boolean;
    never?: boolean;
    optional?: boolean;
    value?: string;
    typescriptType?: 'string' | 'boolean' | 'number';
    fields?: TypedVar[];
    array?: { item: TypedType };
    map?: { key: TypedType, value: TypedType };
    unknown?: boolean;
};

const toDebugSource = (node: M.MichelsonType) => {
    return JSON.stringify(node);
};

const visitContractStorage = (storage: M.MichelsonContractStorage): TypedStorage => {
    const fields = storage.args
        .map(x => visitVar(x))
        .flatMap(x => x);
    return {
        storage: {
            raw: storage as unknown as M.MichelsonType,
            fields: fields,
        },
    };
};

const visitContractParameter = (parameter: M.MichelsonContractParameter): TypedParameter => {
    return {
        methods: parameter.args
            .map(x => visitContractParameterEndpoint(x as MMethod))
            .flatMap(x => x),
    };
};


type MMethod = M.MichelsonTypeOr<[M.MichelsonType, M.MichelsonType]>;
const visitContractParameterEndpoint = (node: MMethod): TypedMethod[] => {
    // console.log('visitContractParameterEndpoint', { node });

    // Sub endpoints (i.e. admin endpoints that are imported)
    if (node.prim === `or`) {
        return node.args.map(x => visitContractParameterEndpoint(x as MMethod)).flatMap(x => x);
    }

    // Sub endpoints as a list (i.e. admin endpoints that are imported)
    if (node.prim === `list` && (node?.args?.[0] as MMethod)?.prim === `or`) {
        return node.args.map(x => visitContractParameterEndpoint(x as MMethod)).flatMap(x => x);
    }

    if (node.annots?.[0]) {
        // A method if it has a name
        const name = node.annots[0];
        if (name.startsWith(`%`)) {
            // console.log('visitContractParameterEndpoint method', { name, node });

            return [{
                name: name.substr(1),
                args: [
                    ...node.prim !== `pair` ? [{ type: visitType(node) }] : [],
                    ...(node.args ?? []).map(x => visitVar(x)),
                ].flatMap(x => x),
            }];
        }
    }

    throw new GenerateApiError(`Unknown method: ${node.prim as string}`, { node });
};


type MVarArgs = M.MichelsonType;
const visitVar = (node: MVarArgs): TypedVar[] => {
    // console.log('visitMethodArgs', { node });
    const debug_source = toDebugSource(node);

    if (typeof node === `string`) {
        return [{
            type: visitType(node),
        }];
    }

    if (`annots` in node && node.annots?.length === 1) {
        // A named arg 
        const name = node.annots[0];
        if (name.startsWith(`%`)) {
            // console.log('visitMethodArgs arg', { name, node });

            return [{
                name: name.substr(1),
                type: visitType(node),
            }];
        }
    }

    if (`prim` in node) {
        if (node.prim === `pair`) {
            return node.args.map(x => visitVar(x as MMethod)).flatMap(x => x);
        }
    }

    // Assume type?
    return [{
        type: visitType(node),
    }];
    // throw new GenerateApiError(`Unknown visitVar node: ${JSON.stringify(node, null, 2)} `, { node });
};

type MType = M.MichelsonType;
const visitType = (node: MType): TypedType => {
    // console.log('visitType', { node });
    const debug_source = toDebugSource(node);

    if (typeof node === `string`) {
        return { raw: node, value: node };
    }

    if (!(`prim` in node)) {
        // Unknown
        console.error(`visitType no prim`, { node });
        return { raw: node, unknown: true };
    }

    // Union
    if (node.prim === `or`) {
        const fields = node.args.map(x => visitVar(x)).flatMap(x => x);
        if (fields.some(x => !x)) {
            throw new GenerateApiError(`or: Some fields are null`, { node });
        }
        return {
            raw: node,
            fields,
        };
    }

    // Intersect
    if (node.prim === `pair`) {
        const fields = node.args.map(x => visitVar(x)).flatMap(x => x);
        if (fields.some(x => !x)) {
            throw new GenerateApiError(`pair: Some fields are null`, { node, args: node.args, fields });
        }
        return {
            raw: node,
            fields,
        };
    }

    // list
    if (node.prim === `list`) {
        const arrayItem = visitType(node.args[0]);
        if (!arrayItem) {
            throw new GenerateApiError(`arrayItem are null`, { node, args: node.args, arrayItem });
        }
        return {
            raw: node,
            array: { item: arrayItem },
        };
    }

    // map
    if (node.prim === `map`
        || node.prim === `big_map`
        || node.prim === `set`
    ) {
        if (node.args.length !== 2) {
            throw new GenerateApiError(`map does not have 2 args`, { node, args: node.args });
        }

        const mapKey = visitType(node.args[0]);
        const mapValue = visitType(node.args[1]);
        if (!mapKey || !mapValue) {
            throw new GenerateApiError(`map is missing key or value`, { node, args: node.args, mapKey, mapValue });
        }
        return {
            raw: node,
            map: {
                key: mapKey,
                value: mapValue,
            },
        };
    }

    // option
    if (node.prim === `option`) {
        return {
            ...visitType(node.args[0]),
            optional: true,
        };
    }

    // boolean
    if (node.prim === `bool`) {
        return {
            raw: node,
            value: node.prim,
            typescriptType: `boolean`,
        };
    }

    // numbers
    if (node.prim === `nat`
        || node.prim === `int`
        || node.prim === `mutez`
        || node.prim === `timestamp`
    ) {
        return {
            raw: node,
            value: node.prim,
            typescriptType: `number`,
        };
    }

    // strings
    if (node.prim === `address`
        || node.prim === `key`
        || node.prim === `key_hash`
        || node.prim === `chain_id`
        || node.prim === `string`
        || node.prim === `signature`
        || node.prim === `ticket`
        || node.prim === `bls12_381_fr`
        || node.prim === `bls12_381_g1`
        || node.prim === `bls12_381_g2`
        || node.prim === `sapling_state`
        || node.prim === `sapling_transaction`
        || node.prim === `contract`
    ) {
        return {
            raw: node,
            value: node.prim,
            typescriptType: `string`,
        };
    }


    // void
    if (node.prim === `unit`) {
        return {
            raw: node,
            unit: true,
        };
    }

    // bytes?
    if (node.prim === `bytes`) {
        return {
            raw: node,
            value: node.prim,
            typescriptType: `string`,
        };
    }

    // misc?
    if (node.prim === `lambda`
        || node.prim === `operation`
    ) {
        return {
            raw: node,
            value: node.prim,
            typescriptType: `string`,
        };
    }


    if (node.prim === `never`
    ) {
        return {
            raw: node,
            never: true,
        };
    }

    // Unknown
    assertExhaustive(node, `Unknown type`);
    throw new GenerateApiError(`Unknown type`, { node });
};

