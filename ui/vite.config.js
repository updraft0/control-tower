import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";

export default defineConfig({
    // FIXME this will not solve the cookie problem
    server: {
        cors: {
            credentials: true
        }
    },
    plugins: [scalaJSPlugin({
        cwd: '../',
        projectID: 'ui'
    })],
});