$(function() {
    $(".expand").click(function() {
        expand($(this));
    });
 });

function expand(o) {
    o.parent().next(".submenu").css("display", "block");
    o.attr("class", "collapse");
    o.click(function() {
        collapse($(this));
    });
}

function collapse(o) {
    o.parent().next(".submenu").css("display", "none");
    o.attr("class", "expand");
    o.click(function() {
        expand(o);
    });
}
