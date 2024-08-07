import { defineConfig } from "vitest/config";
import packageJson from "./package.json";

export default defineConfig({
  test: {
    name: packageJson.name,
    dir: "./src",
    watch: false,
    setupFiles: ["dotenv/config"],
    coverage: { enabled: true, provider: "istanbul", include: ["src/**/*"] },
    typecheck: { enabled: true },
    restoreMocks: true,
  },
});
