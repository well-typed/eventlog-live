"use client";

import { createTheme } from "@mui/material/styles";

const theme = createTheme({
  cssVariables: true,
  breakpoints: {
    values: {
      xs: 0,
      sm: 650,
      md: 1250,
      lg: 1850,
      xl: 2450,
    },
  },
  typography: {
    fontFamily: "var(--font-inter)",
  },
});

export default theme;
