-module(lustre@element@svg).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([animate/1, animate_motion/1, animate_transform/1, mpath/1, set/1, circle/1, ellipse/1, line/1, polygon/1, polyline/1, rect/1, a/2, defs/2, g/2, marker/2, mask/2, missing_glyph/2, pattern/2, svg/2, switch/2, symbol/2, desc/2, metadata/2, title/2, fe_blend/1, fe_color_matrix/1, fe_component_transfer/1, fe_composite/1, fe_convolve_matrix/1, fe_diffuse_lighting/2, fe_displacement_map/1, fe_drop_shadow/1, fe_flood/1, fe_func_a/1, fe_func_b/1, fe_func_g/1, fe_func_r/1, fe_gaussian_blur/1, fe_image/1, fe_merge/2, fe_merge_node/1, fe_morphology/1, fe_offset/1, fe_specular_lighting/2, fe_tile/2, fe_turbulence/1, linear_gradient/2, radial_gradient/2, stop/1, image/1, path/1, text/2, use_/1, fe_distant_light/1, fe_point_light/1, fe_spot_light/1, clip_path/2, script/2, style/2, foreign_object/2, text_path/2, tspan/2]).

-spec animate(list(lustre@internals@vdom:attribute(UEC))) -> lustre@internals@vdom:element(UEC).
animate(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animate"/utf8>>,
        Attrs,
        []
    ).

-spec animate_motion(list(lustre@internals@vdom:attribute(UEG))) -> lustre@internals@vdom:element(UEG).
animate_motion(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateMotion"/utf8>>,
        Attrs,
        []
    ).

-spec animate_transform(list(lustre@internals@vdom:attribute(UEK))) -> lustre@internals@vdom:element(UEK).
animate_transform(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateTransform"/utf8>>,
        Attrs,
        []
    ).

-spec mpath(list(lustre@internals@vdom:attribute(UEO))) -> lustre@internals@vdom:element(UEO).
mpath(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mpath"/utf8>>,
        Attrs,
        []
    ).

-spec set(list(lustre@internals@vdom:attribute(UES))) -> lustre@internals@vdom:element(UES).
set(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"set"/utf8>>,
        Attrs,
        []
    ).

-spec circle(list(lustre@internals@vdom:attribute(UEW))) -> lustre@internals@vdom:element(UEW).
circle(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"circle"/utf8>>,
        Attrs,
        []
    ).

-spec ellipse(list(lustre@internals@vdom:attribute(UFA))) -> lustre@internals@vdom:element(UFA).
ellipse(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"ellipse"/utf8>>,
        Attrs,
        []
    ).

-spec line(list(lustre@internals@vdom:attribute(UFE))) -> lustre@internals@vdom:element(UFE).
line(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"line"/utf8>>,
        Attrs,
        []
    ).

-spec polygon(list(lustre@internals@vdom:attribute(UFI))) -> lustre@internals@vdom:element(UFI).
polygon(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polygon"/utf8>>,
        Attrs,
        []
    ).

-spec polyline(list(lustre@internals@vdom:attribute(UFM))) -> lustre@internals@vdom:element(UFM).
polyline(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polyline"/utf8>>,
        Attrs,
        []
    ).

-spec rect(list(lustre@internals@vdom:attribute(UFQ))) -> lustre@internals@vdom:element(UFQ).
rect(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"rect"/utf8>>,
        Attrs,
        []
    ).

-spec a(
    list(lustre@internals@vdom:attribute(UFU)),
    list(lustre@internals@vdom:element(UFU))
) -> lustre@internals@vdom:element(UFU).
a(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"a"/utf8>>,
        Attrs,
        Children
    ).

-spec defs(
    list(lustre@internals@vdom:attribute(UGA)),
    list(lustre@internals@vdom:element(UGA))
) -> lustre@internals@vdom:element(UGA).
defs(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"defs"/utf8>>,
        Attrs,
        Children
    ).

-spec g(
    list(lustre@internals@vdom:attribute(UGG)),
    list(lustre@internals@vdom:element(UGG))
) -> lustre@internals@vdom:element(UGG).
g(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"g"/utf8>>,
        Attrs,
        Children
    ).

-spec marker(
    list(lustre@internals@vdom:attribute(UGM)),
    list(lustre@internals@vdom:element(UGM))
) -> lustre@internals@vdom:element(UGM).
marker(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"marker"/utf8>>,
        Attrs,
        Children
    ).

-spec mask(
    list(lustre@internals@vdom:attribute(UGS)),
    list(lustre@internals@vdom:element(UGS))
) -> lustre@internals@vdom:element(UGS).
mask(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mask"/utf8>>,
        Attrs,
        Children
    ).

-spec missing_glyph(
    list(lustre@internals@vdom:attribute(UGY)),
    list(lustre@internals@vdom:element(UGY))
) -> lustre@internals@vdom:element(UGY).
missing_glyph(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"missing-glyph"/utf8>>,
        Attrs,
        Children
    ).

-spec pattern(
    list(lustre@internals@vdom:attribute(UHE)),
    list(lustre@internals@vdom:element(UHE))
) -> lustre@internals@vdom:element(UHE).
pattern(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"pattern"/utf8>>,
        Attrs,
        Children
    ).

-spec svg(
    list(lustre@internals@vdom:attribute(UHK)),
    list(lustre@internals@vdom:element(UHK))
) -> lustre@internals@vdom:element(UHK).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-spec switch(
    list(lustre@internals@vdom:attribute(UHQ)),
    list(lustre@internals@vdom:element(UHQ))
) -> lustre@internals@vdom:element(UHQ).
switch(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"switch"/utf8>>,
        Attrs,
        Children
    ).

-spec symbol(
    list(lustre@internals@vdom:attribute(UHW)),
    list(lustre@internals@vdom:element(UHW))
) -> lustre@internals@vdom:element(UHW).
symbol(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"symbol"/utf8>>,
        Attrs,
        Children
    ).

-spec desc(
    list(lustre@internals@vdom:attribute(UIC)),
    list(lustre@internals@vdom:element(UIC))
) -> lustre@internals@vdom:element(UIC).
desc(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"desc"/utf8>>,
        Attrs,
        Children
    ).

-spec metadata(
    list(lustre@internals@vdom:attribute(UII)),
    list(lustre@internals@vdom:element(UII))
) -> lustre@internals@vdom:element(UII).
metadata(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"metadata"/utf8>>,
        Attrs,
        Children
    ).

-spec title(
    list(lustre@internals@vdom:attribute(UIO)),
    list(lustre@internals@vdom:element(UIO))
) -> lustre@internals@vdom:element(UIO).
title(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"title"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_blend(list(lustre@internals@vdom:attribute(UIU))) -> lustre@internals@vdom:element(UIU).
fe_blend(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feBlend"/utf8>>,
        Attrs,
        []
    ).

-spec fe_color_matrix(list(lustre@internals@vdom:attribute(UIY))) -> lustre@internals@vdom:element(UIY).
fe_color_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feColorMatrix"/utf8>>,
        Attrs,
        []
    ).

-spec fe_component_transfer(list(lustre@internals@vdom:attribute(UJC))) -> lustre@internals@vdom:element(UJC).
fe_component_transfer(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComponentTransfer"/utf8>>,
        Attrs,
        []
    ).

-spec fe_composite(list(lustre@internals@vdom:attribute(UJG))) -> lustre@internals@vdom:element(UJG).
fe_composite(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComposite"/utf8>>,
        Attrs,
        []
    ).

-spec fe_convolve_matrix(list(lustre@internals@vdom:attribute(UJK))) -> lustre@internals@vdom:element(UJK).
fe_convolve_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feConvolveMatrix"/utf8>>,
        Attrs,
        []
    ).

-spec fe_diffuse_lighting(
    list(lustre@internals@vdom:attribute(UJO)),
    list(lustre@internals@vdom:element(UJO))
) -> lustre@internals@vdom:element(UJO).
fe_diffuse_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDiffuseLighting"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_displacement_map(list(lustre@internals@vdom:attribute(UJU))) -> lustre@internals@vdom:element(UJU).
fe_displacement_map(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDisplacementMap"/utf8>>,
        Attrs,
        []
    ).

-spec fe_drop_shadow(list(lustre@internals@vdom:attribute(UJY))) -> lustre@internals@vdom:element(UJY).
fe_drop_shadow(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDropShadow"/utf8>>,
        Attrs,
        []
    ).

-spec fe_flood(list(lustre@internals@vdom:attribute(UKC))) -> lustre@internals@vdom:element(UKC).
fe_flood(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFlood"/utf8>>,
        Attrs,
        []
    ).

-spec fe_func_a(list(lustre@internals@vdom:attribute(UKG))) -> lustre@internals@vdom:element(UKG).
fe_func_a(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncA"/utf8>>,
        Attrs,
        []
    ).

-spec fe_func_b(list(lustre@internals@vdom:attribute(UKK))) -> lustre@internals@vdom:element(UKK).
fe_func_b(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncB"/utf8>>,
        Attrs,
        []
    ).

-spec fe_func_g(list(lustre@internals@vdom:attribute(UKO))) -> lustre@internals@vdom:element(UKO).
fe_func_g(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncG"/utf8>>,
        Attrs,
        []
    ).

-spec fe_func_r(list(lustre@internals@vdom:attribute(UKS))) -> lustre@internals@vdom:element(UKS).
fe_func_r(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncR"/utf8>>,
        Attrs,
        []
    ).

-spec fe_gaussian_blur(list(lustre@internals@vdom:attribute(UKW))) -> lustre@internals@vdom:element(UKW).
fe_gaussian_blur(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feGaussianBlur"/utf8>>,
        Attrs,
        []
    ).

-spec fe_image(list(lustre@internals@vdom:attribute(ULA))) -> lustre@internals@vdom:element(ULA).
fe_image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feImage"/utf8>>,
        Attrs,
        []
    ).

-spec fe_merge(
    list(lustre@internals@vdom:attribute(ULE)),
    list(lustre@internals@vdom:element(ULE))
) -> lustre@internals@vdom:element(ULE).
fe_merge(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMerge"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_merge_node(list(lustre@internals@vdom:attribute(ULK))) -> lustre@internals@vdom:element(ULK).
fe_merge_node(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMergeNode"/utf8>>,
        Attrs,
        []
    ).

-spec fe_morphology(list(lustre@internals@vdom:attribute(ULO))) -> lustre@internals@vdom:element(ULO).
fe_morphology(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMorphology"/utf8>>,
        Attrs,
        []
    ).

-spec fe_offset(list(lustre@internals@vdom:attribute(ULS))) -> lustre@internals@vdom:element(ULS).
fe_offset(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feOffset"/utf8>>,
        Attrs,
        []
    ).

-spec fe_specular_lighting(
    list(lustre@internals@vdom:attribute(ULW)),
    list(lustre@internals@vdom:element(ULW))
) -> lustre@internals@vdom:element(ULW).
fe_specular_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpecularLighting"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_tile(
    list(lustre@internals@vdom:attribute(UMC)),
    list(lustre@internals@vdom:element(UMC))
) -> lustre@internals@vdom:element(UMC).
fe_tile(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTile"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_turbulence(list(lustre@internals@vdom:attribute(UMI))) -> lustre@internals@vdom:element(UMI).
fe_turbulence(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTurbulence"/utf8>>,
        Attrs,
        []
    ).

-spec linear_gradient(
    list(lustre@internals@vdom:attribute(UMM)),
    list(lustre@internals@vdom:element(UMM))
) -> lustre@internals@vdom:element(UMM).
linear_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"linearGradient"/utf8>>,
        Attrs,
        Children
    ).

-spec radial_gradient(
    list(lustre@internals@vdom:attribute(UMS)),
    list(lustre@internals@vdom:element(UMS))
) -> lustre@internals@vdom:element(UMS).
radial_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"radialGradient"/utf8>>,
        Attrs,
        Children
    ).

-spec stop(list(lustre@internals@vdom:attribute(UMY))) -> lustre@internals@vdom:element(UMY).
stop(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"stop"/utf8>>,
        Attrs,
        []
    ).

-spec image(list(lustre@internals@vdom:attribute(UNC))) -> lustre@internals@vdom:element(UNC).
image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"image"/utf8>>,
        Attrs,
        []
    ).

-spec path(list(lustre@internals@vdom:attribute(UNG))) -> lustre@internals@vdom:element(UNG).
path(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"path"/utf8>>,
        Attrs,
        []
    ).

-spec text(list(lustre@internals@vdom:attribute(UNK)), binary()) -> lustre@internals@vdom:element(UNK).
text(Attrs, Content) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"text"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-spec use_(list(lustre@internals@vdom:attribute(UNO))) -> lustre@internals@vdom:element(UNO).
use_(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"use"/utf8>>,
        Attrs,
        []
    ).

-spec fe_distant_light(list(lustre@internals@vdom:attribute(UNS))) -> lustre@internals@vdom:element(UNS).
fe_distant_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDistantLight"/utf8>>,
        Attrs,
        []
    ).

-spec fe_point_light(list(lustre@internals@vdom:attribute(UNW))) -> lustre@internals@vdom:element(UNW).
fe_point_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"fePointLight"/utf8>>,
        Attrs,
        []
    ).

-spec fe_spot_light(list(lustre@internals@vdom:attribute(UOA))) -> lustre@internals@vdom:element(UOA).
fe_spot_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpotLight"/utf8>>,
        Attrs,
        []
    ).

-spec clip_path(
    list(lustre@internals@vdom:attribute(UOE)),
    list(lustre@internals@vdom:element(UOE))
) -> lustre@internals@vdom:element(UOE).
clip_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"clipPath"/utf8>>,
        Attrs,
        Children
    ).

-spec script(list(lustre@internals@vdom:attribute(UOK)), binary()) -> lustre@internals@vdom:element(UOK).
script(Attrs, Js) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"script"/utf8>>,
        Attrs,
        [lustre@element:text(Js)]
    ).

-spec style(list(lustre@internals@vdom:attribute(UOO)), binary()) -> lustre@internals@vdom:element(UOO).
style(Attrs, Css) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"style"/utf8>>,
        Attrs,
        [lustre@element:text(Css)]
    ).

-spec foreign_object(
    list(lustre@internals@vdom:attribute(UOS)),
    list(lustre@internals@vdom:element(UOS))
) -> lustre@internals@vdom:element(UOS).
foreign_object(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"foreignObject"/utf8>>,
        Attrs,
        Children
    ).

-spec text_path(
    list(lustre@internals@vdom:attribute(UOY)),
    list(lustre@internals@vdom:element(UOY))
) -> lustre@internals@vdom:element(UOY).
text_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"textPath"/utf8>>,
        Attrs,
        Children
    ).

-spec tspan(
    list(lustre@internals@vdom:attribute(UPE)),
    list(lustre@internals@vdom:element(UPE))
) -> lustre@internals@vdom:element(UPE).
tspan(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"tspan"/utf8>>,
        Attrs,
        Children
    ).
