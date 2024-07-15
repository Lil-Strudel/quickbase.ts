import { defineConfig } from "tsup";

export default defineConfig({
  entry: ["src/*.ts"],
  format: ["cjs", "esm"],
  sourcemap: true,
  clean: true,
  dts: true,
});
