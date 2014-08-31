$(function() {

    // url : String -> String
    var url = function(api) {
        return "http://amutake.me/coq/api/" + api;
    };

    // error : XMLHttpRequest -> StatusCode -> String -> IO ()
    var error = function(xhr, status, text) {
        var err = $.parseJSON(xhr.responseText);
        if (err === null) {
            alert(xhr.responseText);
        } else {
            alert(err.error.message)
        }
    };

    // br : String -> String
    var br = function(str) {
        return str.replace(/\n/g, "<br>");
    }

    var id = -1;
    var evaluated = 0;
    var evaluating = 0;

    var start = function(next) {
        $.ajax({
            url: url("start"),
            type: "POST",
            success: function(info, status, xhr) {
                $("#infoarea").text(br(info.output));
                id = info.id
            },
            error: error,
            complete: next
        });
    };
    var command = function(command, next) {
        var data = {
            command: command
        };
        $.ajax({
            url: url("command/" + id),
            type: "POST",
            contentType: "application/json",
            data: JSON.stringify(data),
            success: function(output, status, xhr) {
                console.log(output);
                if (output.error_output === null) {
                    var outputType = output.last_output.type;
                    if (outputType === "proof") {
                        $("#proofarea").html(br(output.last_output.output));
                    } else {
                        $("#infoarea").html(br(output.last_output.output));
                    }
                } else {
                    $("#infoarea").html(br(output.error_output.output));
                }
            },
            error: error,
            complete: next
        });
    };
    var terminate = function(next) {
        $.ajax({
            url: url("terminate/" + id),
            type: "DELETE",
            error: error,
            complete: next
        });
    };

    start(function() {
        terminate();
    });

    $("#nextbutton").on("click", function() {
        start(function() {
            command($("#codearea").val(), function() {
                terminate();
            });
        });
    });

    // $("#backbutton").on("click", function() {

    // });

    $("#restartbutton").on("click", function() {
        if (id !== -1) {
            terminate();
        }
        start();
    });
});
