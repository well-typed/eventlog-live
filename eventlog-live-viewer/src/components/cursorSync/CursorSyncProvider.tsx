import {
  createContext,
  PropsWithChildren,
  useEffect,
  useId,
  useState,
} from "react";
import uPlot from "uplot";

export interface CursorSyncContext {
  cursorSync: boolean;
  cursorSyncPubSub: uPlot.SyncPubSub | null;
  toggleCursorSync: () => void;
}

export const CursorSyncContext = createContext<CursorSyncContext>({
  cursorSync: true,
  cursorSyncPubSub: null,
  toggleCursorSync: () => {},
});

export default function CursorSyncProvider({
  children,
}: PropsWithChildren<{}>) {
  const [cursorSync, setCursorSync] = useState<boolean>(true);
  const cursorSyncPubSub = uPlot.sync(useId());
  const toggleCursorSync = () => setCursorSync(!cursorSync);
  return (
    <CursorSyncContext.Provider
      value={{
        cursorSync,
        cursorSyncPubSub,
        toggleCursorSync,
      }}
    >
      {children}
    </CursorSyncContext.Provider>
  );
}
