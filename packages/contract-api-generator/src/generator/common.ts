export class GenerateApiError implements Error {
    public name = `GenerateApiError`;
    constructor(public message: string, public readonly data: unknown) {
        console.error(`❌ GenerateApiError: ${message}`, data);
    }
}