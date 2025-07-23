import { useState } from "react";
import { useWallet } from "../hooks/useWallet";
import { buildSignerTransaction } from "../api/buildTransaction";
import { Lucid } from "@lucid-evolution/lucid";

interface MintSignerButtonProps {
  lucidInstance: Awaited<ReturnType<typeof Lucid>> | null;
}

const buttonStyles = {
  base: {
    padding: "0.75rem 1.5rem",
    borderRadius: "8px",
    border: "none",
    color: "white",
    cursor: "pointer",
    transition: "all 0.2s ease-in-out",
    transform: "scale(1)",
    boxShadow: "0 0 0 rgba(0,0,0,0)",
  },
  hover: {
    background: "#1a1f2e",
    transform: "scale(1.02)",
    boxShadow: `0 4px 12px rgba(239, 68, 68, 0.2)`,
  },
  disabled: {
    opacity: 0.7,
    cursor: "not-allowed",
  },
};

const MintSignerButton = ({
  lucidInstance,
}: MintSignerButtonProps) => {
  const { usedAddresses } = useWallet();
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [isHovered, setIsHovered] = useState(false);

  const handleSendAda = async () => {
    if (!usedAddresses?.[0]) {
      setError("Please connect your wallet first");
      return;
    }

    if (!lucidInstance) {
      setError("Lucid not initialized. Please connect your wallet first.");
      return;
    }

    setIsLoading(true);
    setError(null);

    try {
      // Build transaction on the backend - send to self
      const response = await buildSignerTransaction({
        senderAddress: usedAddresses[0],
      });

      // Sign and submit the transaction with the wallet
      const signedTx = await lucidInstance
        .fromTx(response.tx)
        .sign.withWallet()
        .complete();

      const txHash = await signedTx.submit();

      alert("Transaction submitted successfully! " + txHash);
    } catch (err) {
      console.error("Transaction failed:", err);
      setError(err instanceof Error ? err.message : "Transaction failed");
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div
      style={{
        display: "flex",
        flexDirection: "column",
        gap: "1rem",
        alignItems: "center",
      }}
    >
      <button
        onClick={handleSendAda}
        disabled={isLoading}
        onMouseEnter={() => setIsHovered(true)}
        onMouseLeave={() => setIsHovered(false)}
        style={{
          ...buttonStyles.base,
          ...(isHovered && !isLoading ? buttonStyles.hover : {}),
          ...(isLoading ? buttonStyles.disabled : {}),
        }}
      >
        {isLoading ? "Sending..." : "Become Signer"}
      </button>

      {error && (
        <p style={{ color: "#ef4444", marginTop: "1rem" }}>Error: {error}</p>
      )}
    </div>
  );
};

export default MintSignerButton;
