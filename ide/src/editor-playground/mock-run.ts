// Each result corresponds to the index of the chunks
export default function run(chunks: string[]): {chunkLength: number, programLength: number}[] {
  const total = chunks.join('\n');
  return chunks.map((chunk) => (
    { chunkLength: chunk.length, programLength: total.length }
  ));
}
