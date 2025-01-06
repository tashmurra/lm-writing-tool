import { diffChars, diffWords } from "diff";

interface Correction {
    start: number;
    end: number;
    toInsert: string;
}

export function applyCorrections(text: string, corrections: Correction[]): string {
    let result = text;
    let offset = 0;
    for (const c of corrections) {
        result = result.substring(0, c.start + offset) + c.toInsert + result.substring(c.end + offset);
        offset += c.toInsert.length - (c.end - c.start);
    }
    return result;
}
/**
 * Calculate the corrections needed to transform `original` into `corrected`.
 * @param original - The original text.
 * @param corrected - The corrected text.
 * @returns List of corrections.
 */
export function calculateCorrections(original: string, corrected: string): Correction[] {
    const diffs = diffChars(original, corrected);
    const corrections: Correction[] = [];
    let currentIndex = 0;
    let pendingRemoval: Correction | null = null;

    for (const diff of diffs) {
        if (diff.added) {
            if (pendingRemoval) {
                // Merge removal and addition into a single edit
                corrections.push({
                    start: pendingRemoval.start,
                    end: pendingRemoval.end,
                    toInsert: diff.value,
                });
                pendingRemoval = null;
            } else {
                corrections.push({
                    start: currentIndex,
                    end: currentIndex,
                    toInsert: diff.value,
                });
            }
        } else if (diff.removed) {
            const length = diff.value.length;
            if (pendingRemoval) {
                // Extend the pending removal
                pendingRemoval.end += length;
            } else {
                pendingRemoval = {
                    start: currentIndex,
                    end: currentIndex + length,
                    toInsert: "",
                };
            }
            currentIndex += length;
        } else {
            if (pendingRemoval) {
                // Commit any pending removal before moving on
                corrections.push(pendingRemoval);
                pendingRemoval = null;
            }
            currentIndex += diff.value.length;
        }
    }

    // Commit any remaining pending removal
    if (pendingRemoval) {
        corrections.push(pendingRemoval);
    }

    const sourceWords = original.split(/\s+/);
    const sourceWordIndices: { start: number, end: number }[] = [];
    let start = 0;
    for (const w of sourceWords) {
        sourceWordIndices.push({ start: start, end: start + w.length });
        start += w.length + 1;
    }
    const wordCorrections: Correction[] = [];
    for (const wi of sourceWordIndices) {
        const changesInWord = corrections.filter(c => c.start >= wi.start && c.end <= wi.end);
        let correctedWord = applyCorrections(original.substring(wi.start, wi.end), changesInWord.map(c=>({start: c.start - wi.start, end: c.end - wi.start, toInsert: c.toInsert})));
        if (correctedWord !== original.substring(wi.start, wi.end)) {
            wordCorrections.push({
                start: wi.start,
                end: wi.end,
                toInsert: correctedWord
            });
        }
    }


    return wordCorrections;
}
/**
 * 
 * @param text The original text.
 * @param index The index of the character.
 * @returns The line and column of the character. The line and column are 0-based.
 */
export function getLineCol(text: string, index: number): { line: number, col: number } {
    let line = 0;
    let col = 0;
    for (let i = 0; i < index; i++) {
        if (text[i] === '\n') {
            line++;
            col = 0;
        } else {
            col++;
        }
    }
    return { line, col };
}