This project was bootstrapped with [Create React App](https://github.com/facebook/create-react-app).

## Compiling and Running

Run `npm run web` with `pyret-lang/` as your current directory. Then `cd` into `ide/` and run `npm install`.
Next, proceed with either `npm start` or `npm run build`.

### `npm start`

Runs the app in the development mode.<br>
Open [http://localhost:3000](http://localhost:3000) to view it in the browser.

The page will reload if you make edits.<br>
You will also see any lint errors in the console.

### `npm run build`

Builds the app for production to the `build` folder.<br>
It correctly bundles React in production mode and optimizes the build for the best performance.

The build is minified and the filenames include the hashes.<br>
Your app is ready to be deployed!

See the section about [deployment](https://facebook.github.io/create-react-app/docs/deployment) for more information.

## Linting

Both of the build commands will lint the project against the [Airbnb Javascript Style Guide](https://github.com/airbnb/javascript). Use `npx eslint src/<filename>` while inside the `ide` directory to lint a single file. ESLint will automatically fix some minor style issues when passed the `--fix` flag like so: `npx eslint --fix <src/filename>`.
