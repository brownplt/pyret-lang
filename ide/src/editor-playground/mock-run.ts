// Each result corresponds to the index of the chunks
export default function run(chunks: string[]): string[] {
  const total = chunks.join('\n');
  return chunks.map((chunk) => (
    `Chunk length: ${chunk.length} (Total length: ${total.length})`
  ));
}
