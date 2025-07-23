import { useWallet } from "../hooks/useWallet";
import { useCallback, useEffect, useRef, useState } from "react";
import { Lucid } from "@lucid-evolution/lucid";

const WalletConnect = () => {
  const { isConnected, initLucid } = useWallet();
  const hasLoggedAddress = useRef(false);
  const [, setWalletAddress] = useState<string | null>(null);
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const [lucidInstance, setLucidInstance] = useState<Awaited<
    ReturnType<typeof Lucid>
  > | null>(null);

  const initialize = useCallback(async () => {
    try {
      const instance = await initLucid();
      if (instance) {
        setLucidInstance(instance);
      }
    } catch (error) {
      console.error("Error initializing Lucid:", error);
    }
  }, [initLucid]);

  useEffect(() => {
    if (isConnected) {
      initialize();
    } else {
      setWalletAddress(null);
      setLucidInstance(null);
      hasLoggedAddress.current = false;
    }
  }, [isConnected, initialize]);

  return (
    <div>
    </div>
  );
};

export default WalletConnect;
