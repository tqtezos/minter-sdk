import { OpFilter, FilterExpression, Filter, OperationContent } from './interface';
export declare const evaluateOpFilter: (op: OperationContent, filter: OpFilter) => boolean | undefined;
export declare const evaluateExpression: (op: OperationContent, exp: FilterExpression) => boolean;
export declare const evaluateFilter: (op: OperationContent, filter: Filter) => boolean;
