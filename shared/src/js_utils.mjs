export function validate_date(year,month,day) {
    const d = new Date();
    if (isNan(d.setYear(year))) return false;
    if (isNan(d.setMonth(month))) return false;
    if (isNan(d.setDate(day))) return false;
    return d.getYear() == year && d.getMonth() == month && d.getDate() == day;
}

export function join_paths(parts) {
     return parts.join("/")  ;
}
export function full_url(path) {
    path = path.replace(/^\//, "");
    return window.location.origin + "/" + path;
}
