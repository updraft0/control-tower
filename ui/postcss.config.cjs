module.exports = () => {
    return {
        plugins: [
            require('postcss-import'),
            require('postcss-simple-vars'),
            require('autoprefixer')(),
            require("postcss-nesting"),
            require('postcss-reporter'),
            require('postcss-fail-on-warn')
        ],
    };
};