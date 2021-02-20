/**
 * Casts object/array items to BigNumber
 * keys support lodash path notation
 * @param data input object or array
 * @param keys keys for processing or all items if not defined
 *
 * @see https://lodash.com/docs/#get
 *
 */
export declare function castToBigNumber(data: any, keys?: any): object;
/**
 * Casts object/array BigNumber items to strings for readability
 * @param data input object or array
 * @param keys keys for processing or all items if not defined
 *
 */
export declare function castToString(data: any, keys?: any): object;
