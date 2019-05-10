var wms_layers = [];
var format_LocalMunicipalities_0 = new ol.format.GeoJSON();
var features_LocalMunicipalities_0 = format_LocalMunicipalities_0.readFeatures(json_LocalMunicipalities_0, 
            {dataProjection: 'EPSG:4326', featureProjection: 'EPSG:3857'});
var jsonSource_LocalMunicipalities_0 = new ol.source.Vector({
    attributions: [new ol.Attribution({html: '<a href=""></a>'})],
});
jsonSource_LocalMunicipalities_0.addFeatures(features_LocalMunicipalities_0);var lyr_LocalMunicipalities_0 = new ol.layer.Vector({
                declutter: true,
                source:jsonSource_LocalMunicipalities_0, 
                style: style_LocalMunicipalities_0,
                title: '<img src="styles/legend/LocalMunicipalities_0.png" /> LocalMunicipalities'
            });

lyr_LocalMunicipalities_0.setVisible(true);
var layersList = [lyr_LocalMunicipalities_0];
lyr_LocalMunicipalities_0.set('fieldAliases', {'OBJECTID': 'OBJECTID', 'ProvinceCo': 'ProvinceCo', 'ProvinceNa': 'ProvinceNa', 'LocalMunic': 'LocalMunic', 'LocalMun_1': 'LocalMun_1', 'DistrictMu': 'DistrictMu', 'District_1': 'District_1', 'Year': 'Year', 'Shape__Are': 'Shape__Are', 'Shape__Len': 'Shape__Len', });
lyr_LocalMunicipalities_0.set('fieldImages', {'OBJECTID': 'TextEdit', 'ProvinceCo': 'TextEdit', 'ProvinceNa': 'TextEdit', 'LocalMunic': 'TextEdit', 'LocalMun_1': 'TextEdit', 'DistrictMu': 'TextEdit', 'District_1': 'TextEdit', 'Year': 'TextEdit', 'Shape__Are': 'TextEdit', 'Shape__Len': 'TextEdit', });
lyr_LocalMunicipalities_0.set('fieldLabels', {'OBJECTID': 'no label', 'ProvinceCo': 'no label', 'ProvinceNa': 'no label', 'LocalMunic': 'no label', 'LocalMun_1': 'no label', 'DistrictMu': 'no label', 'District_1': 'no label', 'Year': 'no label', 'Shape__Are': 'no label', 'Shape__Len': 'no label', });
lyr_LocalMunicipalities_0.on('precompose', function(evt) {
    evt.context.globalCompositeOperation = 'normal';
});