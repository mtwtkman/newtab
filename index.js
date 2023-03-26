const STORAGE_KEY = "newtab-bookmarks"

function getValue(key) {
  return localStorage.getItem(key) || null
}

function getJsonValue(key, defaultValue) {
  const value = getValue(key)
  if (value === null) { return defaultValue }
  return JSON.parse(value)
}

function setJsonValue(key, value) {
  localStorage.setItem(key, JSON.stringify(value))
}

function fetchBookmarks() {
  return getJsonValue(STORAGE_KEY, [])
}

function saveBookmarks(value) {
  return setJsonValue(STORAGE_KEY, value)
}

function exportBookmarks() {
  const data = getValue(STORAGE_KEY)
  const blob = new Blob([data], { type: "application/json" })
  const url = URL.createObjectURL(blob)
  const link = document.createElement("a")
  link.setAttribute("href", url)
  link.setAttribute("download", "newtab-bookmarks.json")
  link.style.display = "none"
  link.click()
  URL.revokeObjectURL(url)
}

window.App = function(regionId) {
  const node = document.getElementById(regionId)
  const app = Elm.Main.init({
    node,
    flags: fetchBookmarks(),
  })
  app.ports.updateBookmarks.subscribe(message => {
    saveBookmarks(message)
    app.ports.messageReceiver.send(fetchBookmarks())
  })
  app.ports.exportBookmarks.subscribe(_message => {
    exportBookmarks()
  })
}
