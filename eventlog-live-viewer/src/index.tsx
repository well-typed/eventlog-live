import { createRoot } from "react-dom/client";
import App from "./components/app";
import { CssBaseline, ThemeProvider } from "@mui/material";
import { createTheme } from "@mui/material/styles";

const theme = createTheme({
  colorSchemes: {
    dark: true,
  },
  breakpoints: {
    values: {
      xs: 0,
      sm: 650,
      md: 1250,
      lg: 1850,
      xl: 2450,
    },
  },
});

// Render the React app in the new app root
const root = createRoot(document.getElementById("app")!);
root.render(
  <ThemeProvider theme={theme}>
    <CssBaseline />
    <App />
  </ThemeProvider>,
);
