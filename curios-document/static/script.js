// The one thing a page cannot do for itself: a new page's rail starts at the top, so the module it is about is scrolled into view. Nothing else runs, and a page is complete without this.
(function () {
    var current = document.querySelector(".tree a.current");
    if (current) {
        current.scrollIntoView({ block: "center" });
    }
})();
