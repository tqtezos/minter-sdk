export declare const MANAGER_LAMBDA: {
    setDelegate: (key: string) => ({
        prim: string;
        args?: undefined;
    } | {
        prim: string;
        args: ({
            prim: string;
            string?: undefined;
        } | {
            string: string;
            prim?: undefined;
        })[];
    })[];
    removeDelegate: () => ({
        prim: string;
        args?: undefined;
    } | {
        prim: string;
        args: {
            prim: string;
        }[];
    })[];
    transferImplicit: (key: string, mutez: number) => ({
        prim: string;
        args?: undefined;
    } | {
        prim: string;
        args: ({
            prim: string;
            string?: undefined;
        } | {
            string: string;
            prim?: undefined;
        })[];
    } | {
        prim: string;
        args: ({
            prim: string;
            int?: undefined;
        } | {
            int: string;
            prim?: undefined;
        })[];
    })[];
    transferToContract: (key: string, amount: number) => ({
        prim: string;
        args?: undefined;
    } | {
        prim: string;
        args: ({
            prim: string;
            string?: undefined;
        } | {
            string: string;
            prim?: undefined;
        })[];
    } | {
        prim: string;
        args: {
            prim: string;
        }[][][];
    }[] | {
        prim: string;
        args: ({
            prim: string;
            int?: undefined;
        } | {
            int: string;
            prim?: undefined;
        })[];
    })[];
};
