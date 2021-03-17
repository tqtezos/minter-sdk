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

export const getTypeAliasesCode = (): string => {
    return `
import { MichelsonMap } from '@taquito/taquito';
import { BigNumber } from 'bignumber.js';

export type address = string & { __type: 'address' };
export type bytes = string & { __type: 'bytes' };
export type contract = string & { __type: 'contract' };
export type timestamp = string & { __type: 'timestamp' };

export type int = BigNumber & { __type: 'int' };
export type nat = BigNumber & { __type: 'nat' };

export type mutez = BigNumber & { __type: 'mutez' };
export type tez = BigNumber & { __type: 'tez' };

export type MMap<K, V> = Omit<MichelsonMap<K, V>, 'get'> & { get: (key: K) => V };
export type BigMap<K, V> = Omit<MichelsonMap<K, V>, 'get'> & { get: (key: K) => Promise<V> };
    `;
};

const toTypescriptCode = (storage: TypedStorage, methods: TypedMethod[]): {
    final: string;
    typeMapping: string;
    storage: string;
    methods: string;
} => {
    type StrictType = { strictType: string, baseType?: string, raw?: string };
    const usedStrictTypes = [] as StrictType[];
    const addStrictType = (strictType: StrictType) => {
        if (!usedStrictTypes.some(x => x.strictType === strictType.strictType)) {
            usedStrictTypes.push(strictType);
        }
    };

    // Not really tabs :)
    const tabs = (indent: number) => Array(indent).fill(`    `).join(``);
    const toIndentedItems = (indent: number, delimeters: { afterItem?: string, beforeItem?: string }, items: string[]) => {
        return `
${tabs(indent + 1)}${items.join(`${delimeters.afterItem ?? ``}
${tabs(indent + 1)}${delimeters.beforeItem ?? ``}`)}
${tabs(indent)}`;
    };

    const typeToCode = (t: TypedType, indent: number): string => {
        if (t.typescriptType) {
            //return `${t.typescriptType}`;

            const prim = `prim` in t.raw ? t.raw.prim : `unknown`;

            // Strict mode
            if (t.typescriptType === `boolean`
                || t.typescriptType === `string` && prim === `string`
            ) {
                return `${t.typescriptType}`;
            }

            const baseType = t.typescriptType === `number` ? `BigNumber` : t.typescriptType;
            const strictType = { baseType, strictType: prim };
            addStrictType(strictType);

            return strictType.strictType;
        }
        if (t.array) {
            return `${typeToCode(t.array.item, indent)}[]`;
        }
        if (t.map) {

            const strictType = t.map.isBigMap
                ? { strictType: `BigMap`, raw: `type BigMap<K, V> = Omit<MichelsonMap<K, V>, 'get'> & { get: (key: K) => Promise<V> }` }
                : { strictType: `MMap`, raw: `type MMap<K, V> = MichelsonMap<K, V>` };
            addStrictType(strictType);

            return `${strictType.strictType}<${typeToCode(t.map.key, indent)}, ${typeToCode(t.map.value, indent)}>`;
        }
        if (t.fields) {
            return `{${toIndentedItems(indent, {},
                t.fields.map((a, i) => varToCode(a, i, indent + 1) + `;`),
            )}}`;
        }
        if (t.union) {

            const getUnionItem = (a: TypedVar, i: number) => {
                const itemCode = `${varToCode(a, i, indent + 1)}`;

                // Keep on single line if already on single line
                if (!itemCode.includes(`\n`)) {
                    return `{ ${itemCode} }`;
                }

                // Indent if multi-line (and remake with extra indent)
                return `{${toIndentedItems(indent + 1, {}, [`${varToCode(a, i, indent + 2)}`])}}`;
            };

            return `(${toIndentedItems(indent, { beforeItem: `| ` },
                t.union.map(getUnionItem),
            )})`;
        }
        if (t.unit) {
            const strictType = { baseType: `(true | undefined)`, strictType: `unit` };
            addStrictType(strictType);
            return strictType.strictType;
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

        return `params: {${toIndentedItems(indent, {},
            args.filter(x => x.name || !x.type.unit).map((a, i) => varToCode(a, i, indent + 1) + `;`),
        )}}`;
    };

    const methodsToCode = (indent: number) => {
        const methodFields = methods.map(x => {
            const methodCode = `${x.name}: (${argsToCode(x.args, indent + 1)}) => Promise<void>;`;
            return methodCode;
        });

        const methodsTypeCode = `type Methods = {${toIndentedItems(indent, {}, methodFields)}};`;
        return methodsTypeCode;
    };

    const storageToCode = (indent: number) => {
        const storageTypeCode = `type Storage = ${typeToCode(storage.storage, indent)};`;
        return storageTypeCode;
    };

    const methodsCode = methodsToCode(0);
    const storageCode = storageToCode(0);

    const typeMapping = usedStrictTypes
        .sort((a, b) => a.strictType.localeCompare(b.strictType))
        .map(x => {
            if (x.baseType) {
                return `type ${x.strictType} = ${x.baseType} & { __type: '${x.strictType}' };`;
            }
            if (x.raw) {
                return `${x.raw};`;
            }
            return `// type ${x.strictType} = unknown;`;
        }).join(`\n`);

    //     const typeAliases = `
    // import { MichelsonMap } from '@taquito/taquito';
    // import { BigNumber } from 'bignumber.js';

    // ${typeMapping}
    //         `.trim();
    const typeAliases = `import { ${usedStrictTypes.map(x => x.strictType).join(`, `)} } from './type-aliases';`;

    const finalCode = `
${typeAliases}

${storageCode}

${methodsCode}

export type Contract = { methods: Methods, storage: Storage };
`;
    return {
        final: finalCode,
        storage: storageCode,
        methods: methodsCode,
        typeMapping,
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
    union?: TypedVar[];
    array?: { item: TypedType };
    map?: { key: TypedType, value: TypedType, isBigMap: boolean };
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
                    ...node.prim !== `pair` && !node.args ? [{ type: visitType(node) }] : [],
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
        const union = node.args.map(x => visitVar(x)).flatMap(x => x);

        // Flatten
        const rightSide = union[1];
        if (rightSide.type.union) {
            union.pop();
            union.push(...rightSide.type.union);
        }

        if (union.some(x => !x)) {
            throw new GenerateApiError(`or: Some fields are null`, { node });
        }
        return {
            raw: node,
            union,
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
    if (node.prim === `list`
        || node.prim === `set`
    ) {
        if (node.args.length !== 1) {
            throw new GenerateApiError(`list does not have 1 arg`, { node, args: node.args });
        }

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
                isBigMap: node.prim === `big_map`,
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
        || node.prim === `timestamp`

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

