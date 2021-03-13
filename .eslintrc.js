module.exports = {
    root: true,
    parser: '@typescript-eslint/parser',
    parserOptions: {
        project: "./tsconfig.json",
    },
    plugins: [
        '@typescript-eslint',
        'import-quotes',
        'jest',
        `promise`,
    ],
    extends: [
        'eslint:recommended',
        'plugin:@typescript-eslint/recommended',
        'plugin:@typescript-eslint/recommended-requiring-type-checking',
        'plugin:jest/recommended',
        `plugin:promise/recommended`,
    ],
    rules: {
        // Always require semicolon
        "semi": ["error", "always"],

        // Backtick Quotes Everywhere (except jsx and imports)
        "quotes": ["off", "backtick"],
        "@typescript-eslint/quotes": ["error", "backtick"],
        "jsx-quotes": ["error", "prefer-single"],
        "import-quotes/import-quotes": ["error", "single"],

        // Always Comma Dangle with multiline
        "comma-dangle": ["error", {
            "arrays": "always-multiline",
            "objects": "always-multiline",
            "imports": "always-multiline",
            "exports": "always-multiline",
            "functions": "always-multiline",
        }],

        // Use semicolons for multiline types, comma for single line types
        "@typescript-eslint/member-delimiter-style": ["error", {
            "multiline": {
                "delimiter": "semi",
                "requireLast": true
            },
            "singleline": {
                "delimiter": "comma",
                "requireLast": false
            }
        }],
    }
};