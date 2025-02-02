import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";
import dns from 'dns'

dns.setDefaultResultOrder('verbatim');


export default defineConfig({
    server: {
        port: 8091,
        strictPort: true,
        proxy: {
            '/api': {
                preserveHeaderKeyCase: true,
                autoRewrite: true,
                secure: false,
                // localAddress: '127.0.0.1',
                // this can be unreliable if localhost resolves to an ipv6 address
                target: 'http://localhost:8092',
                ws: true
            }
        },
        cors: {
            origin: true,
            credentials: true
        }
    },
    plugins: [scalaJSPlugin({
        cwd: '../',
        projectID: 'ui'
    })],
});
