module.exports = () => {
    return {
        plugins: [
            require('postcss-import'),
            require('postcss-mixins'),
            require('postcss-simple-vars'),
            require("postcss-nesting"),
            require('autoprefixer')(),
            require('postcss-reporter'),
            require('postcss-fail-on-warn'),
            require('postcss-hexrgba')
        ],
    };
};