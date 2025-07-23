export function hexToString(hex: string): string {
  // Split into byte-sized chunks, parse, and convert to characters
  return hex.match(/.{1,2}/g)
    ?.map((byte) => String.fromCharCode(parseInt(byte, 16)))
    .join('') ?? ''
}