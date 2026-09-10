"use client";

import { createContext, PropsWithChildren, useState } from "react";

export interface RefreshContext {
  refresh: boolean;
  toggleRefresh: () => void;
}

export const RefreshContext = createContext<RefreshContext>({
  refresh: true,
  toggleRefresh: () => {},
});

function RefreshProvider({ children }: PropsWithChildren<{}>) {
  const [refresh, setRefresh] = useState<boolean>(true);
  const toggleRefresh = () => setRefresh(!refresh);
  return (
    <RefreshContext.Provider value={{ refresh, toggleRefresh }}>
      {children}
    </RefreshContext.Provider>
  );
}

export default RefreshProvider;
