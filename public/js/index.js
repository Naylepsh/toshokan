/**
 * @param {string} configTemplateElemPath
 * @param {string} targetParentPath
 */
const addScrapingConfig = (configTemplateElemPath, targetParentPath) => {
  const template = document.querySelector(configTemplateElemPath)
  const parent = document.querySelector(targetParentPath)
  if (template && parent) {
    const newConfigElem = template.cloneNode(true)
    newConfigElem.removeAttribute('id')


    parent.appendChild(newConfigElem)
  }
}
