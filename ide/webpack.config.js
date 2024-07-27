// Generated using webpack-cli https://github.com/webpack/webpack-cli

const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyPlugin = require('copy-webpack-plugin');
const webpack = require('webpack');

const isProduction = process.env.NODE_ENV == 'production';


const config = {
    entry: './src/index.tsx',
    // There are specific warnings of this form:
    // WARNING in ../node_modules/babel-core/lib/transformation/file/options/option-manager.js 178:19-37
    // Critical dependency: the request of a dependency is an expression
    // 
    // They cause no issues and are deep in a babel dependency, so we ignore them
    ignoreWarnings: [/the request of a dependency is an expression/],
    output: {
        path: path.resolve(__dirname, 'build'),
        library: "embedableParley",
        filename: 'static/js/bundle[name].js',
        // There are also additional JS chunk files if you use code splitting.
        chunkFilename: 'static/js/[name].chunk.js',
    },
    devServer: {
        open: true,
        host: 'localhost',
    },
    plugins: [
        // Add your plugins here
        // Learn more about plugins from https://webpack.js.org/configuration/plugins/
        new HtmlWebpackPlugin({
          template: 'public/index.html'
        }),
        new CopyPlugin({
          patterns: [
            { from: '../build/worker/pyret.js', to: './' },
          ]
        }),
        new webpack.ProvidePlugin({
          process: 'process/browser',
        }),
        new webpack.ProvidePlugin({
          Buffer: ['buffer', 'Buffer'],
        }),
        new webpack.ProvidePlugin({
          path: 'path',
        }),
    ],
    module: {
        rules: [
            {
                test: /\.(ts|tsx)$/i,
                loader: 'ts-loader',
                exclude: ['/node_modules/'],
            },
            {
                test: /\.css$/i,
                use: ['style-loader','css-loader','import-glob-loader'],
            },
            {
                test: /\.(eot|svg|ttf|woff|woff2|png|jpg|gif)$/i,
                type: 'asset',
            },

            // Add your rules for custom modules here
            // Learn more about loaders from https://webpack.js.org/loaders/
        ],
    },
    resolve: {
        extensions: ['.tsx', '.ts', '.jsx', '.js', '...'],
        fallback: {
          crypto: require.resolve("crypto-browserify"),
          tty: require.resolve("tty-browserify"),
          stream: require.resolve("stream-browserify"),
          process: require.resolve("process"),
          buffer: require.resolve("buffer"),
          path: require.resolve("path-browserify"),
          module: false,
          dgram: false,
          dns: false,
          fs: false,
          http2: false,
          net: false,
          tls: false,
          child_process: false,
          vm: false,
        }
    },
};

module.exports = () => {
    if (isProduction) {
        config.mode = 'production';
        
        
    } else {
        config.mode = 'development';
    }
    return config;
};
