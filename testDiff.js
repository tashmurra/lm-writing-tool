const { diffChars } = require("diff");

/**
 * Calculate the corrections needed to transform `original` into `corrected`.
 * @param {string} original - The original text.
 * @param {string} corrected - The corrected text.
 * @returns {Array<{start: number, end: number, toInsert: string}>} - List of corrections.
 */
function calculateCorrections(original, corrected) {
  const diffs = diffChars(original, corrected);
  const corrections = [];
  let currentIndex = 0;
  let pendingRemoval = null;

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

  return corrections;
}

// Example usage:
const original = "The quick brown fox jumps over the lazy dog.";
const corrected = "The very quick brown cat leaps over the super lazy dog.";
const corrections = calculateCorrections(original, corrected);
console.log(corrections);