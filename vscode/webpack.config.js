/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

//@ts-check
'use strict';

//@ts-check
/** @typedef {import('webpack').Configuration} WebpackConfig **/

const path = require('path');
const webpack = require('webpack');
const CopyPlugin = require('copy-webpack-plugin');

/** @type WebpackConfig */
const webExtensionConfig = {
	mode: 'none', // this leaves the source code as close as possible to the original (when packaging we set this to 'production')
	target: 'webworker', // extensions run in a webworker context
	entry: {
		'extension': './src/webExtension.ts'
	},
	output: {
		filename: '[name].js',
		path: path.join(__dirname, './dist/web'),
		libraryTarget: 'commonjs',
		devtoolModuleFilenameTemplate: '../../[resource-path]'
	},
	resolve: {
		mainFields: ['browser', 'module', 'main'], // look for `browser` entry point in imported node modules
		extensions: ['.ts', '.js'], // support ts-files and js-files
		alias: {
			// provides alternate implementation for node module and source files
		},
		fallback: {
			// Webpack 5 no longer polyfills Node.js core modules automatically.
			// see https://webpack.js.org/configuration/resolve/#resolvefallback
			// for the list of Node.js core module polyfills.
			'assert': require.resolve('assert'),
			path: require.resolve('path-browserify')
		}
	},
	module: {
		rules: [{
			test: /\.ts$/,
			exclude: /node_modules/,
			use: [{
				loader: 'ts-loader'
			}]
		},
		{
			test: /\.html/,
			type: 'asset/source'
		}
		]
	},
	plugins: [
		new webpack.optimize.LimitChunkCountPlugin({
			maxChunks: 1 // disable chunks by default since web extensions must be a single bundle
		}),
		new webpack.ProvidePlugin({
			process: 'process/browser', // provide a shim for the global `process` variable
		}),
		new CopyPlugin({
			patterns: [
				{
					from: path.resolve(__dirname, "build"),
					to: "./build",
					globOptions: {
						// Only the gz artifacts are fetched (and inflated in-page by
						// beforePyret): cpo-main.jarr.gz.js, and in the ts flavor
						// cpo-main-ts.jarr.gz.js + ts-compiler.gz.js. Drop the
						// uncompressed bundles and both flavors' big intermediates,
						// which also keeps every shipped file under Open VSX's ~15MB
						// cap (the ts intermediates are ~20MB EACH when a `make
						// web-ts` build preceded packaging).
						ignore: [
							"**/snap/**",
							"**/js/cpo-main.jarr",
							"**/js/cpo-main.jarr.js",
							"**/js/cpo-main.jarr.min",
							"**/js/cpo-main-ts.jarr",
							"**/js/cpo-main-ts.jarr.js",
							"**/js/cpo-main-ts.jarr.min",
							"**/js/ts-compiler.js",
						],
					},
					// Terser skip this file for minification
					info: { minimized: true },
				},
			]
		})
	],
	externals: {
		'vscode': 'commonjs vscode', // ignored because it doesn't exist
	},
	performance: {
		hints: false
	},
	devtool: 'nosources-source-map', // create a source map that points to the original source file
	infrastructureLogging: {
		level: "log", // enables logging required for problem matchers
	},
};

module.exports = [ webExtensionConfig ];