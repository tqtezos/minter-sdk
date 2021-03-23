module.exports = {
  root: true,
  parser: '@typescript-eslint/parser',
  plugins: [
    '@typescript-eslint',
    'jest',
  ],
  extends: [
    'eslint:recommended',
    'plugin:@typescript-eslint/recommended',
    'prettier',
  ],
  env: {
    es6: true,
    jest: true,
    node: true,
  },
  rules: {
    "semi": ["error", "always"],
    "@typescript-eslint/no-explicit-any": "off",
    "@typescript-eslint/explicit-module-boundary-types": "off",
    "comma-dangle": ["warn", {
      "arrays": "always-multiline",
      "objects": "always-multiline",
      "imports": "always-multiline",
      "exports": "always-multiline",
      "functions": "always-multiline",
    }],
    "object-curly-spacing": ["warn", "always"],
    "array-bracket-spacing": ["warn", "never"],
    "eol-last": ["warn", "always"],
    "no-trailing-spaces": "warn",
    "no-multiple-empty-lines": "warn",
    "max-len": ["warn", {
      code: 120,
      ignoreStrings: true,
      ignoreTemplateLiterals: true,
      tabWidth: 2,
    }],
    "indent": [2, 2],
    "no-tabs": "warn",
  },
};
