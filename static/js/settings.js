
$(document).ready(function() {
    $("#viewfield-add-form").ajaxForm({
        success: jsonForm("#viewfield-add-inner")
    });
})
