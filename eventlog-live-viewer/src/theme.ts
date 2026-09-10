"use client";

import { createTheme } from "@mui/material/styles";

const theme = createTheme({
  cssVariables: true,
  breakpoints: {
    values: {
      xs: 0,
      sm: 600,
      md: 1200,
      lg: 1800,
      xl: 2400,
    },
  },
  typography: {
    fontFamily: "var(--font-inter)",
  },
});

export default theme;
