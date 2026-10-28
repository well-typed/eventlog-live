import { Command } from "@commander-js/extra-typings";
import * as esbuild from "esbuild";
import svgr from "esbuild-plugin-svgr";
import copyStaticFiles from "esbuild-copy-static-files";

const program = new Command()
  .name("build")
  .option("--analyze")
  .option("--watch")
  .option("--outdir <directory>")
  .action(async ({ analyze, watch, outdir = "dist" }) => {
    const config = {
      entryPoints: ["src/index.tsx"],
      bundle: true,
      outdir: outdir,
      alias: {
        react: "preact/compat",
        "react-dom": "preact/compat",
      },
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
    if (!watch) {
      /************************************************************************/
      /* Build for Production                                                 */
      /************************************************************************/
      const result = await esbuild.build({
        ...config,
        metafile: analyze,
        minify: true,
      });
      if (result.metafile !== undefined) {
        console.log(await esbuild.analyzeMetafile(result.metafile));
      }
    } else {
      /************************************************************************/
      /* Serve with Live Reload                                               */
      /************************************************************************/
      const context = await esbuild.context({
        ...config,
        sourcemap: true,
        define: { IS_DEVELOPMENT: "true" },
      });
      await context.watch();
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
    }
  });

program.parse();
