/**
 * @param {string} templatePath
 * @param {string} targetPath
 */
const loadTemplate = (templatePath, targetPath) => {
  const template = document.querySelector(templatePath);
  const parent = document.querySelector(targetPath);
  if (!template) {
    throw new Error(`${templatePath} was not found in DOM`);
  }
  if (template.tagName !== "TEMPLATE") {
    throw new Error(`${templatePath}'s element is not a template`);
  }

  if (!parent) {
    throw new Error(`${targetPath} was not found in DOM`);
  }

  const newConfigElem = template.content.cloneNode(true);
  parent.appendChild(newConfigElem);
  // This is needed to make HTMX attributes work on newly added elements
  htmx.process(document.body);
};

/**
 * @param {Element} self
 * @param {string} selector
 */
const removeClosest = (self, selector) => {
  const elem = self.closest(selector);
  if (elem) {
    elem.remove();
  }
};

/**
 * Collects checked asset IDs (or uses provided IDs) and opens the merge preview modal.
 * @param {string} authorId
 * @param {number[]} [preselectedIds]
 */
const openMergePreview = (authorId, preselectedIds) => {
  const ids =
    preselectedIds ||
    Array.from(
      document.querySelectorAll("input[name=merge-asset]:checked")
    ).map((cb) => parseInt(cb.value));

  if (ids.length < 2) {
    alert("Select at least 2 assets to merge");
    return;
  }

  fetch("/authors/" + authorId + "/merge/preview", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ assetIds: ids }),
  })
    .then((r) => r.text())
    .then((html) => {
      document.getElementById("merge-modal-content").innerHTML = html;
      document.getElementById("merge_modal").showModal();
    });
};

/**
 * Confirms the merge: moves all entries from non-target assets into the target.
 * @param {string} authorId
 * @param {number[]} assetIds
 */
const confirmMerge = (authorId, assetIds) => {
  const targetId = parseInt(
    document.querySelector("input[name=merge-target]:checked").value
  );
  const sourceIds = assetIds.filter((id) => id !== targetId);

  fetch("/authors/" + authorId + "/merge", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ assetIds: sourceIds, targetId: targetId }),
  }).then(() => {
    window.location = "/authors/" + authorId;
  });
};
