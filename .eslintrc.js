module.exports = {
    root: true,
    parser: '@typescript-eslint/parser',
    parserOptions: {
        project: "./tsconfig.json",
    },
    plugins: [
        '@typescript-eslint',
        `promise`,
        'jest',
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
    }
};