export function full_url(path) {
    path = path.replace("^/", path);
    return window.location.origin + "/" + path;
}
