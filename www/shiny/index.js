$(document).ready(function(){
    var href = window.location.href.match(new RegExp(window.location.protocol + "\/\/" + window.location.hostname + "\/analysis\/([^\/]*)\/([^\/]*)\/?([^\.]*)?.html"));

    if(href != null) {
        if (href[3] != undefined) {
            var domainX = href[1].split(":")[0];
            var domainY = href[2].split(":")[0];
            var domainZ = href[3].split(":")[0];
            var dX = href[1].split(":")[1];
            var dY = href[2].split(":")[1];
            var rP = href[3].split(":")[1];

            var datasetX = "http://" + domainX + ".270a.info/dataset/" + dX
            var datasetY = "http://" + domainY + ".270a.info/dataset/" + dY
            var refPeriod = "http://reference.data.gov.uk/id/" + domainZ + "/" + rP

            $("#datasetX").val(datasetX);
            $("#datasetY").val(datasetY);
            $("#refPeriod").val(refPeriod);

            $(".entry-content").css("background", "url(/theme/default/images/icons/icon_loading.gif) no-repeat 65% 100px");
        }
        else {
            var domainX = href[1].split(":")[0];
            var dX = href[1].split(":")[1];
            var rA = href[2].split(",");

            var datasetX = "http://" + domainX + ".270a.info/dataset/" + dX
            var refArea = rA

            $("#datasetX").val(datasetX);
//            $("#refArea").val(refArea);

            $(".entry-content").css("background", "url(/theme/default/images/icons/icon_loading.gif) no-repeat 65% 100px");
        }
    }

    $(".form_regression #submit").click(function() {
        var datasetX = $('#datasetX').val().match(/http:\/\/([^.]*).270a.info\/dataset\/(.*)/);
        var datasetY = $('#datasetY').val().match(/http:\/\/([^.]*).270a.info\/dataset\/(.*)/);
        var refPeriod = $('#refPeriod').val().match(/http:\/\/reference.data.gov.uk\/id\/([^\/]*)\/(.*)/);

        port = '';
        if (window.location.port.length > 0) { port = ":" + window.location.port }

        window.location.href = window.location.protocol + "//" + window.location.hostname + port + "/analysis/" +
            datasetX[1] + ":" + datasetX[2] + "/" +
            datasetY[1] + ":" + datasetY[2] + "/" +
            refPeriod[1] + ":" + refPeriod[2];
    });

    $(".form_timeseries #submit").click(function() {
        var datasetX = $('#datasetX').val().match(/http:\/\/([^.]*).270a.info\/dataset\/(.*)/);
        var refArea = $('#refArea').val().match(/([0-9A-Z]{2,3})(,([0-9A-Z]{2,3},)*([0-9A-Z]{2,3}))?/);

        port = '';
        if (window.location.port.length > 0) { port = ":" + window.location.port }

        window.location.href = window.location.protocol + "//" + window.location.hostname + port + "/analysis/" +
            datasetX[1] + ":" + datasetX[2] + "/" +
            refArea[0]
    });

    $('#download-csv a').on('click', function() {
        $("body").removeClass();
    });
});

