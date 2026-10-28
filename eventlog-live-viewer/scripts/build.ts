import { Command } from "@commander-js/extra-typings";
import * as esbuild from "esbuild";
import svgr from "esbuild-plugin-svgr";
import copyStaticFiles from "esbuild-copy-static-files";

const program = new Command()
  .name("build")
  .option("--watch")
  .option("--outdir <directory>")
  .action(async ({ watch, outdir = "dist" }) => {
    const config = {
      entryPoints: ["src/index.tsx"],
      bundle: true,
      minify: true,
      outdir: outdir,
      plugins: [
        svgr({
          svgoConfig: {
            plugins: [
              {
                name: "preset-default",
                params: {
                  overrides: {
                    inlineStyles: false,
                    minifyStyles: false,
                  },
                },
              },
            ],
          },
        }),
        copyStaticFiles({
          src: "./src/public",
          dest: outdir,
          errorOnExist: false,
        }),
      ],
    };
    if (watch) {
      const context = await esbuild.context({
        ...config,
        sourcemap: true,
        define: {
          EVENTLOG_LIVE_VIEWER_WEBSOCKET_URL: "'ws://localhost:30179'",
        },
      });
      context.watch();
      const { hosts, port } = await context.serve({
        servedir: "dist",
      });
      console.log("Serving files from 'dist'");
      console.log("----------------------------------------");
      for (const [k, host] of hosts.entries()) {
        const p = k === 0 ? "   Local:" : "External:";
        console.log(`  ${p} http://${host}:${port}`);
      }
      console.log("----------------------------------------");
    } else {
      await esbuild.build(config);
    }
  });

program.parse();
