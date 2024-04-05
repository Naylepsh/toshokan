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
