function jsonForm (innerSel, onSuccess) {
    return function (data) {
        if (data.status === 'ok') {
            if (onSuccess) {
                onSuccess()
            } else {
                location.reload()
            }
        } else {
            $(innerSel).html(data.form)
        }
    }
}
