-module(lustre@element@svg).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([animate/1, animate_motion/1, animate_transform/1, mpath/1, set/1, circle/1, ellipse/1, line/1, polygon/1, polyline/1, rect/1, a/2, defs/2, g/2, marker/2, mask/2, missing_glyph/2, pattern/2, svg/2, switch/2, symbol/2, desc/2, metadata/2, title/2, fe_blend/1, fe_color_matrix/1, fe_component_transfer/1, fe_composite/1, fe_convolve_matrix/1, fe_diffuse_lighting/2, fe_displacement_map/1, fe_drop_shadow/1, fe_flood/1, fe_func_a/1, fe_func_b/1, fe_func_g/1, fe_func_r/1, fe_gaussian_blur/1, fe_image/1, fe_merge/2, fe_merge_node/1, fe_morphology/1, fe_offset/1, fe_specular_lighting/2, fe_tile/2, fe_turbulence/1, linear_gradient/2, radial_gradient/2, stop/1, image/1, path/1, text/2, use_/1, fe_distant_light/1, fe_point_light/1, fe_spot_light/1, clip_path/2, script/2, style/2, foreign_object/2, text_path/2, tspan/2]).

-spec animate(list(lustre@internals@vdom:attribute(RBS))) -> lustre@internals@vdom:element(RBS).
animate(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animate"/utf8>>,
        Attrs,
        []
    ).

-spec animate_motion(list(lustre@internals@vdom:attribute(RBW))) -> lustre@internals@vdom:element(RBW).
animate_motion(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateMotion"/utf8>>,
        Attrs,
        []
    ).

-spec animate_transform(list(lustre@internals@vdom:attribute(RCA))) -> lustre@internals@vdom:element(RCA).
animate_transform(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateTransform"/utf8>>,
        Attrs,
        []
    ).

-spec mpath(list(lustre@internals@vdom:attribute(RCE))) -> lustre@internals@vdom:element(RCE).
mpath(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mpath"/utf8>>,
        Attrs,
        []
    ).

-spec set(list(lustre@internals@vdom:attribute(RCI))) -> lustre@internals@vdom:element(RCI).
set(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"set"/utf8>>,
        Attrs,
        []
    ).

-spec circle(list(lustre@internals@vdom:attribute(RCM))) -> lustre@internals@vdom:element(RCM).
circle(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"circle"/utf8>>,
        Attrs,
        []
    ).

-spec ellipse(list(lustre@internals@vdom:attribute(RCQ))) -> lustre@internals@vdom:element(RCQ).
ellipse(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"ellipse"/utf8>>,
        Attrs,
        []
    ).

-spec line(list(lustre@internals@vdom:attribute(RCU))) -> lustre@internals@vdom:element(RCU).
line(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"line"/utf8>>,
        Attrs,
        []
    ).

-spec polygon(list(lustre@internals@vdom:attribute(RCY))) -> lustre@internals@vdom:element(RCY).
polygon(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polygon"/utf8>>,
        Attrs,
        []
    ).

-spec polyline(list(lustre@internals@vdom:attribute(RDC))) -> lustre@internals@vdom:element(RDC).
polyline(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polyline"/utf8>>,
        Attrs,
        []
    ).

-spec rect(list(lustre@internals@vdom:attribute(RDG))) -> lustre@internals@vdom:element(RDG).
rect(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"rect"/utf8>>,
        Attrs,
        []
    ).

-spec a(
    list(lustre@internals@vdom:attribute(RDK)),
    list(lustre@internals@vdom:element(RDK))
) -> lustre@internals@vdom:element(RDK).
a(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"a"/utf8>>,
        Attrs,
        Children
    ).

-spec defs(
    list(lustre@internals@vdom:attribute(RDQ)),
    list(lustre@internals@vdom:element(RDQ))
) -> lustre@internals@vdom:element(RDQ).
defs(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"defs"/utf8>>,
        Attrs,
        Children
    ).

-spec g(
    list(lustre@internals@vdom:attribute(RDW)),
    list(lustre@internals@vdom:element(RDW))
) -> lustre@internals@vdom:element(RDW).
g(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"g"/utf8>>,
        Attrs,
        Children
    ).

-spec marker(
    list(lustre@internals@vdom:attribute(REC)),
    list(lustre@internals@vdom:element(REC))
) -> lustre@internals@vdom:element(REC).
marker(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"marker"/utf8>>,
        Attrs,
        Children
    ).

-spec mask(
    list(lustre@internals@vdom:attribute(REI)),
    list(lustre@internals@vdom:element(REI))
) -> lustre@internals@vdom:element(REI).
mask(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mask"/utf8>>,
        Attrs,
        Children
    ).

-spec missing_glyph(
    list(lustre@internals@vdom:attribute(REO)),
    list(lustre@internals@vdom:element(REO))
) -> lustre@internals@vdom:element(REO).
missing_glyph(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"missing-glyph"/utf8>>,
        Attrs,
        Children
    ).

-spec pattern(
    list(lustre@internals@vdom:attribute(REU)),
    list(lustre@internals@vdom:element(REU))
) -> lustre@internals@vdom:element(REU).
pattern(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"pattern"/utf8>>,
        Attrs,
        Children
    ).

-spec svg(
    list(lustre@internals@vdom:attribute(RFA)),
    list(lustre@internals@vdom:element(RFA))
) -> lustre@internals@vdom:element(RFA).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-spec switch(
    list(lustre@internals@vdom:attribute(RFG)),
    list(lustre@internals@vdom:element(RFG))
) -> lustre@internals@vdom:element(RFG).
switch(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"switch"/utf8>>,
        Attrs,
        Children
    ).

-spec symbol(
    list(lustre@internals@vdom:attribute(RFM)),
    list(lustre@internals@vdom:element(RFM))
) -> lustre@internals@vdom:element(RFM).
symbol(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"symbol"/utf8>>,
        Attrs,
        Children
    ).

-spec desc(
    list(lustre@internals@vdom:attribute(RFS)),
    list(lustre@internals@vdom:element(RFS))
) -> lustre@internals@vdom:element(RFS).
desc(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"desc"/utf8>>,
        Attrs,
        Children
    ).

-spec metadata(
    list(lustre@internals@vdom:attribute(RFY)),
    list(lustre@internals@vdom:element(RFY))
) -> lustre@internals@vdom:element(RFY).
metadata(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"metadata"/utf8>>,
        Attrs,
        Children
    ).

-spec title(
    list(lustre@internals@vdom:attribute(RGE)),
    list(lustre@internals@vdom:element(RGE))
) -> lustre@internals@vdom:element(RGE).
title(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"title"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_blend(list(lustre@internals@vdom:attribute(RGK))) -> lustre@internals@vdom:element(RGK).
fe_blend(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feBlend"/utf8>>,
        Attrs,
        []
    ).

-spec fe_color_matrix(list(lustre@internals@vdom:attribute(RGO))) -> lustre@internals@vdom:element(RGO).
fe_color_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feColorMatrix"/utf8>>,
        Attrs,
        []
    ).

-spec fe_component_transfer(list(lustre@internals@vdom:attribute(RGS))) -> lustre@internals@vdom:element(RGS).
fe_component_transfer(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComponentTransfer"/utf8>>,
        Attrs,
        []
    ).

-spec fe_composite(list(lustre@internals@vdom:attribute(RGW))) -> lustre@internals@vdom:element(RGW).
fe_composite(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComposite"/utf8>>,
        Attrs,
        []
    ).

-spec fe_convolve_matrix(list(lustre@internals@vdom:attribute(RHA))) -> lustre@internals@vdom:element(RHA).
fe_convolve_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feConvolveMatrix"/utf8>>,
        Attrs,
        []
    ).

-spec fe_diffuse_lighting(
    list(lustre@internals@vdom:attribute(RHE)),
    list(lustre@internals@vdom:element(RHE))
) -> lustre@internals@vdom:element(RHE).
fe_diffuse_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDiffuseLighting"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_displacement_map(list(lustre@internals@vdom:attribute(RHK))) -> lustre@internals@vdom:element(RHK).
fe_displacement_map(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDisplacementMap"/utf8>>,
        Attrs,
        []
    ).

-spec fe_drop_shadow(list(lustre@internals@vdom:attribute(RHO))) -> lustre@internals@vdom:element(RHO).
fe_drop_shadow(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDropShadow"/utf8>>,
        Attrs,
        []
    ).

-spec fe_flood(list(lustre@internals@vdom:attribute(RHS))) -> lustre@internals@vdom:element(RHS).
fe_flood(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFlood"/utf8>>,
        Attrs,
        []
    ).

-spec fe_func_a(list(lustre@internals@vdom:attribute(RHW))) -> lustre@internals@vdom:element(RHW).
fe_func_a(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncA"/utf8>>,
        Attrs,
        []
    ).

-spec fe_func_b(list(lustre@internals@vdom:attribute(RIA))) -> lustre@internals@vdom:element(RIA).
fe_func_b(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncB"/utf8>>,
        Attrs,
        []
    ).

-spec fe_func_g(list(lustre@internals@vdom:attribute(RIE))) -> lustre@internals@vdom:element(RIE).
fe_func_g(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncG"/utf8>>,
        Attrs,
        []
    ).

-spec fe_func_r(list(lustre@internals@vdom:attribute(RII))) -> lustre@internals@vdom:element(RII).
fe_func_r(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncR"/utf8>>,
        Attrs,
        []
    ).

-spec fe_gaussian_blur(list(lustre@internals@vdom:attribute(RIM))) -> lustre@internals@vdom:element(RIM).
fe_gaussian_blur(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feGaussianBlur"/utf8>>,
        Attrs,
        []
    ).

-spec fe_image(list(lustre@internals@vdom:attribute(RIQ))) -> lustre@internals@vdom:element(RIQ).
fe_image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feImage"/utf8>>,
        Attrs,
        []
    ).

-spec fe_merge(
    list(lustre@internals@vdom:attribute(RIU)),
    list(lustre@internals@vdom:element(RIU))
) -> lustre@internals@vdom:element(RIU).
fe_merge(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMerge"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_merge_node(list(lustre@internals@vdom:attribute(RJA))) -> lustre@internals@vdom:element(RJA).
fe_merge_node(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMergeNode"/utf8>>,
        Attrs,
        []
    ).

-spec fe_morphology(list(lustre@internals@vdom:attribute(RJE))) -> lustre@internals@vdom:element(RJE).
fe_morphology(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMorphology"/utf8>>,
        Attrs,
        []
    ).

-spec fe_offset(list(lustre@internals@vdom:attribute(RJI))) -> lustre@internals@vdom:element(RJI).
fe_offset(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feOffset"/utf8>>,
        Attrs,
        []
    ).

-spec fe_specular_lighting(
    list(lustre@internals@vdom:attribute(RJM)),
    list(lustre@internals@vdom:element(RJM))
) -> lustre@internals@vdom:element(RJM).
fe_specular_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpecularLighting"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_tile(
    list(lustre@internals@vdom:attribute(RJS)),
    list(lustre@internals@vdom:element(RJS))
) -> lustre@internals@vdom:element(RJS).
fe_tile(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTile"/utf8>>,
        Attrs,
        Children
    ).

-spec fe_turbulence(list(lustre@internals@vdom:attribute(RJY))) -> lustre@internals@vdom:element(RJY).
fe_turbulence(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTurbulence"/utf8>>,
        Attrs,
        []
    ).

-spec linear_gradient(
    list(lustre@internals@vdom:attribute(RKC)),
    list(lustre@internals@vdom:element(RKC))
) -> lustre@internals@vdom:element(RKC).
linear_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"linearGradient"/utf8>>,
        Attrs,
        Children
    ).

-spec radial_gradient(
    list(lustre@internals@vdom:attribute(RKI)),
    list(lustre@internals@vdom:element(RKI))
) -> lustre@internals@vdom:element(RKI).
radial_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"radialGradient"/utf8>>,
        Attrs,
        Children
    ).

-spec stop(list(lustre@internals@vdom:attribute(RKO))) -> lustre@internals@vdom:element(RKO).
stop(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"stop"/utf8>>,
        Attrs,
        []
    ).

-spec image(list(lustre@internals@vdom:attribute(RKS))) -> lustre@internals@vdom:element(RKS).
image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"image"/utf8>>,
        Attrs,
        []
    ).

-spec path(list(lustre@internals@vdom:attribute(RKW))) -> lustre@internals@vdom:element(RKW).
path(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"path"/utf8>>,
        Attrs,
        []
    ).

-spec text(list(lustre@internals@vdom:attribute(RLA)), binary()) -> lustre@internals@vdom:element(RLA).
text(Attrs, Content) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"text"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-spec use_(list(lustre@internals@vdom:attribute(RLE))) -> lustre@internals@vdom:element(RLE).
use_(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"use"/utf8>>,
        Attrs,
        []
    ).

-spec fe_distant_light(list(lustre@internals@vdom:attribute(RLI))) -> lustre@internals@vdom:element(RLI).
fe_distant_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDistantLight"/utf8>>,
        Attrs,
        []
    ).

-spec fe_point_light(list(lustre@internals@vdom:attribute(RLM))) -> lustre@internals@vdom:element(RLM).
fe_point_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"fePointLight"/utf8>>,
        Attrs,
        []
    ).

-spec fe_spot_light(list(lustre@internals@vdom:attribute(RLQ))) -> lustre@internals@vdom:element(RLQ).
fe_spot_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpotLight"/utf8>>,
        Attrs,
        []
    ).

-spec clip_path(
    list(lustre@internals@vdom:attribute(RLU)),
    list(lustre@internals@vdom:element(RLU))
) -> lustre@internals@vdom:element(RLU).
clip_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"clipPath"/utf8>>,
        Attrs,
        Children
    ).

-spec script(list(lustre@internals@vdom:attribute(RMA)), binary()) -> lustre@internals@vdom:element(RMA).
script(Attrs, Js) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"script"/utf8>>,
        Attrs,
        [lustre@element:text(Js)]
    ).

-spec style(list(lustre@internals@vdom:attribute(RME)), binary()) -> lustre@internals@vdom:element(RME).
style(Attrs, Css) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"style"/utf8>>,
        Attrs,
        [lustre@element:text(Css)]
    ).

-spec foreign_object(
    list(lustre@internals@vdom:attribute(RMI)),
    list(lustre@internals@vdom:element(RMI))
) -> lustre@internals@vdom:element(RMI).
foreign_object(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"foreignObject"/utf8>>,
        Attrs,
        Children
    ).

-spec text_path(
    list(lustre@internals@vdom:attribute(RMO)),
    list(lustre@internals@vdom:element(RMO))
) -> lustre@internals@vdom:element(RMO).
text_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"textPath"/utf8>>,
        Attrs,
        Children
    ).

-spec tspan(
    list(lustre@internals@vdom:attribute(RMU)),
    list(lustre@internals@vdom:element(RMU))
) -> lustre@internals@vdom:element(RMU).
tspan(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"tspan"/utf8>>,
        Attrs,
        Children
    ).
