-module(lustre@element@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([html/2, text/1, base/1, head/2, link/1, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, hgroup/2, main/2, nav/2, section/2, search/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, data/2, dfn/2, em/2, i/2, kbd/2, mark/2, q/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var/2, wbr/1, area/1, audio/2, img/1, map/2, track/1, video/2, embed/1, iframe/1, object/1, picture/2, portal/1, source/1, svg/2, math/2, canvas/1, noscript/2, script/2, del/2, ins/2, caption/2, col/1, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/2, output/2, progress/2, select/2, textarea/2, details/2, dialog/2, summary/2, slot/1, template/2]).

-spec html(
    list(lustre@internals@vdom:attribute(SYJ)),
    list(lustre@internals@vdom:element(SYJ))
) -> lustre@internals@vdom:element(SYJ).
html(Attrs, Children) ->
    lustre@element:element(<<"html"/utf8>>, Attrs, Children).

-spec text(binary()) -> lustre@internals@vdom:element(any()).
text(Content) ->
    lustre@element:text(Content).

-spec base(list(lustre@internals@vdom:attribute(SYR))) -> lustre@internals@vdom:element(SYR).
base(Attrs) ->
    lustre@element:element(<<"base"/utf8>>, Attrs, []).

-spec head(
    list(lustre@internals@vdom:attribute(SYV)),
    list(lustre@internals@vdom:element(SYV))
) -> lustre@internals@vdom:element(SYV).
head(Attrs, Children) ->
    lustre@element:element(<<"head"/utf8>>, Attrs, Children).

-spec link(list(lustre@internals@vdom:attribute(SZB))) -> lustre@internals@vdom:element(SZB).
link(Attrs) ->
    lustre@element:element(<<"link"/utf8>>, Attrs, []).

-spec meta(list(lustre@internals@vdom:attribute(SZF))) -> lustre@internals@vdom:element(SZF).
meta(Attrs) ->
    lustre@element:element(<<"meta"/utf8>>, Attrs, []).

-spec style(list(lustre@internals@vdom:attribute(SZJ)), binary()) -> lustre@internals@vdom:element(SZJ).
style(Attrs, Css) ->
    lustre@element:element(<<"style"/utf8>>, Attrs, [text(Css)]).

-spec title(list(lustre@internals@vdom:attribute(SZN)), binary()) -> lustre@internals@vdom:element(SZN).
title(Attrs, Content) ->
    lustre@element:element(<<"title"/utf8>>, Attrs, [text(Content)]).

-spec body(
    list(lustre@internals@vdom:attribute(SZR)),
    list(lustre@internals@vdom:element(SZR))
) -> lustre@internals@vdom:element(SZR).
body(Attrs, Children) ->
    lustre@element:element(<<"body"/utf8>>, Attrs, Children).

-spec address(
    list(lustre@internals@vdom:attribute(SZX)),
    list(lustre@internals@vdom:element(SZX))
) -> lustre@internals@vdom:element(SZX).
address(Attrs, Children) ->
    lustre@element:element(<<"address"/utf8>>, Attrs, Children).

-spec article(
    list(lustre@internals@vdom:attribute(TAD)),
    list(lustre@internals@vdom:element(TAD))
) -> lustre@internals@vdom:element(TAD).
article(Attrs, Children) ->
    lustre@element:element(<<"article"/utf8>>, Attrs, Children).

-spec aside(
    list(lustre@internals@vdom:attribute(TAJ)),
    list(lustre@internals@vdom:element(TAJ))
) -> lustre@internals@vdom:element(TAJ).
aside(Attrs, Children) ->
    lustre@element:element(<<"aside"/utf8>>, Attrs, Children).

-spec footer(
    list(lustre@internals@vdom:attribute(TAP)),
    list(lustre@internals@vdom:element(TAP))
) -> lustre@internals@vdom:element(TAP).
footer(Attrs, Children) ->
    lustre@element:element(<<"footer"/utf8>>, Attrs, Children).

-spec header(
    list(lustre@internals@vdom:attribute(TAV)),
    list(lustre@internals@vdom:element(TAV))
) -> lustre@internals@vdom:element(TAV).
header(Attrs, Children) ->
    lustre@element:element(<<"header"/utf8>>, Attrs, Children).

-spec h1(
    list(lustre@internals@vdom:attribute(TBB)),
    list(lustre@internals@vdom:element(TBB))
) -> lustre@internals@vdom:element(TBB).
h1(Attrs, Children) ->
    lustre@element:element(<<"h1"/utf8>>, Attrs, Children).

-spec h2(
    list(lustre@internals@vdom:attribute(TBH)),
    list(lustre@internals@vdom:element(TBH))
) -> lustre@internals@vdom:element(TBH).
h2(Attrs, Children) ->
    lustre@element:element(<<"h2"/utf8>>, Attrs, Children).

-spec h3(
    list(lustre@internals@vdom:attribute(TBN)),
    list(lustre@internals@vdom:element(TBN))
) -> lustre@internals@vdom:element(TBN).
h3(Attrs, Children) ->
    lustre@element:element(<<"h3"/utf8>>, Attrs, Children).

-spec h4(
    list(lustre@internals@vdom:attribute(TBT)),
    list(lustre@internals@vdom:element(TBT))
) -> lustre@internals@vdom:element(TBT).
h4(Attrs, Children) ->
    lustre@element:element(<<"h4"/utf8>>, Attrs, Children).

-spec h5(
    list(lustre@internals@vdom:attribute(TBZ)),
    list(lustre@internals@vdom:element(TBZ))
) -> lustre@internals@vdom:element(TBZ).
h5(Attrs, Children) ->
    lustre@element:element(<<"h5"/utf8>>, Attrs, Children).

-spec h6(
    list(lustre@internals@vdom:attribute(TCF)),
    list(lustre@internals@vdom:element(TCF))
) -> lustre@internals@vdom:element(TCF).
h6(Attrs, Children) ->
    lustre@element:element(<<"h6"/utf8>>, Attrs, Children).

-spec hgroup(
    list(lustre@internals@vdom:attribute(TCL)),
    list(lustre@internals@vdom:element(TCL))
) -> lustre@internals@vdom:element(TCL).
hgroup(Attrs, Children) ->
    lustre@element:element(<<"hgroup"/utf8>>, Attrs, Children).

-spec main(
    list(lustre@internals@vdom:attribute(TCR)),
    list(lustre@internals@vdom:element(TCR))
) -> lustre@internals@vdom:element(TCR).
main(Attrs, Children) ->
    lustre@element:element(<<"main"/utf8>>, Attrs, Children).

-spec nav(
    list(lustre@internals@vdom:attribute(TCX)),
    list(lustre@internals@vdom:element(TCX))
) -> lustre@internals@vdom:element(TCX).
nav(Attrs, Children) ->
    lustre@element:element(<<"nav"/utf8>>, Attrs, Children).

-spec section(
    list(lustre@internals@vdom:attribute(TDD)),
    list(lustre@internals@vdom:element(TDD))
) -> lustre@internals@vdom:element(TDD).
section(Attrs, Children) ->
    lustre@element:element(<<"section"/utf8>>, Attrs, Children).

-spec search(
    list(lustre@internals@vdom:attribute(TDJ)),
    list(lustre@internals@vdom:element(TDJ))
) -> lustre@internals@vdom:element(TDJ).
search(Attrs, Children) ->
    lustre@element:element(<<"search"/utf8>>, Attrs, Children).

-spec blockquote(
    list(lustre@internals@vdom:attribute(TDP)),
    list(lustre@internals@vdom:element(TDP))
) -> lustre@internals@vdom:element(TDP).
blockquote(Attrs, Children) ->
    lustre@element:element(<<"blockquote"/utf8>>, Attrs, Children).

-spec dd(
    list(lustre@internals@vdom:attribute(TDV)),
    list(lustre@internals@vdom:element(TDV))
) -> lustre@internals@vdom:element(TDV).
dd(Attrs, Children) ->
    lustre@element:element(<<"dd"/utf8>>, Attrs, Children).

-spec 'div'(
    list(lustre@internals@vdom:attribute(TEB)),
    list(lustre@internals@vdom:element(TEB))
) -> lustre@internals@vdom:element(TEB).
'div'(Attrs, Children) ->
    lustre@element:element(<<"div"/utf8>>, Attrs, Children).

-spec dl(
    list(lustre@internals@vdom:attribute(TEH)),
    list(lustre@internals@vdom:element(TEH))
) -> lustre@internals@vdom:element(TEH).
dl(Attrs, Children) ->
    lustre@element:element(<<"dl"/utf8>>, Attrs, Children).

-spec dt(
    list(lustre@internals@vdom:attribute(TEN)),
    list(lustre@internals@vdom:element(TEN))
) -> lustre@internals@vdom:element(TEN).
dt(Attrs, Children) ->
    lustre@element:element(<<"dt"/utf8>>, Attrs, Children).

-spec figcaption(
    list(lustre@internals@vdom:attribute(TET)),
    list(lustre@internals@vdom:element(TET))
) -> lustre@internals@vdom:element(TET).
figcaption(Attrs, Children) ->
    lustre@element:element(<<"figcaption"/utf8>>, Attrs, Children).

-spec figure(
    list(lustre@internals@vdom:attribute(TEZ)),
    list(lustre@internals@vdom:element(TEZ))
) -> lustre@internals@vdom:element(TEZ).
figure(Attrs, Children) ->
    lustre@element:element(<<"figure"/utf8>>, Attrs, Children).

-spec hr(list(lustre@internals@vdom:attribute(TFF))) -> lustre@internals@vdom:element(TFF).
hr(Attrs) ->
    lustre@element:element(<<"hr"/utf8>>, Attrs, []).

-spec li(
    list(lustre@internals@vdom:attribute(TFJ)),
    list(lustre@internals@vdom:element(TFJ))
) -> lustre@internals@vdom:element(TFJ).
li(Attrs, Children) ->
    lustre@element:element(<<"li"/utf8>>, Attrs, Children).

-spec menu(
    list(lustre@internals@vdom:attribute(TFP)),
    list(lustre@internals@vdom:element(TFP))
) -> lustre@internals@vdom:element(TFP).
menu(Attrs, Children) ->
    lustre@element:element(<<"menu"/utf8>>, Attrs, Children).

-spec ol(
    list(lustre@internals@vdom:attribute(TFV)),
    list(lustre@internals@vdom:element(TFV))
) -> lustre@internals@vdom:element(TFV).
ol(Attrs, Children) ->
    lustre@element:element(<<"ol"/utf8>>, Attrs, Children).

-spec p(
    list(lustre@internals@vdom:attribute(TGB)),
    list(lustre@internals@vdom:element(TGB))
) -> lustre@internals@vdom:element(TGB).
p(Attrs, Children) ->
    lustre@element:element(<<"p"/utf8>>, Attrs, Children).

-spec pre(
    list(lustre@internals@vdom:attribute(TGH)),
    list(lustre@internals@vdom:element(TGH))
) -> lustre@internals@vdom:element(TGH).
pre(Attrs, Children) ->
    lustre@element:element(<<"pre"/utf8>>, Attrs, Children).

-spec ul(
    list(lustre@internals@vdom:attribute(TGN)),
    list(lustre@internals@vdom:element(TGN))
) -> lustre@internals@vdom:element(TGN).
ul(Attrs, Children) ->
    lustre@element:element(<<"ul"/utf8>>, Attrs, Children).

-spec a(
    list(lustre@internals@vdom:attribute(TGT)),
    list(lustre@internals@vdom:element(TGT))
) -> lustre@internals@vdom:element(TGT).
a(Attrs, Children) ->
    lustre@element:element(<<"a"/utf8>>, Attrs, Children).

-spec abbr(
    list(lustre@internals@vdom:attribute(TGZ)),
    list(lustre@internals@vdom:element(TGZ))
) -> lustre@internals@vdom:element(TGZ).
abbr(Attrs, Children) ->
    lustre@element:element(<<"abbr"/utf8>>, Attrs, Children).

-spec b(
    list(lustre@internals@vdom:attribute(THF)),
    list(lustre@internals@vdom:element(THF))
) -> lustre@internals@vdom:element(THF).
b(Attrs, Children) ->
    lustre@element:element(<<"b"/utf8>>, Attrs, Children).

-spec bdi(
    list(lustre@internals@vdom:attribute(THL)),
    list(lustre@internals@vdom:element(THL))
) -> lustre@internals@vdom:element(THL).
bdi(Attrs, Children) ->
    lustre@element:element(<<"bdi"/utf8>>, Attrs, Children).

-spec bdo(
    list(lustre@internals@vdom:attribute(THR)),
    list(lustre@internals@vdom:element(THR))
) -> lustre@internals@vdom:element(THR).
bdo(Attrs, Children) ->
    lustre@element:element(<<"bdo"/utf8>>, Attrs, Children).

-spec br(list(lustre@internals@vdom:attribute(THX))) -> lustre@internals@vdom:element(THX).
br(Attrs) ->
    lustre@element:element(<<"br"/utf8>>, Attrs, []).

-spec cite(
    list(lustre@internals@vdom:attribute(TIB)),
    list(lustre@internals@vdom:element(TIB))
) -> lustre@internals@vdom:element(TIB).
cite(Attrs, Children) ->
    lustre@element:element(<<"cite"/utf8>>, Attrs, Children).

-spec code(
    list(lustre@internals@vdom:attribute(TIH)),
    list(lustre@internals@vdom:element(TIH))
) -> lustre@internals@vdom:element(TIH).
code(Attrs, Children) ->
    lustre@element:element(<<"code"/utf8>>, Attrs, Children).

-spec data(
    list(lustre@internals@vdom:attribute(TIN)),
    list(lustre@internals@vdom:element(TIN))
) -> lustre@internals@vdom:element(TIN).
data(Attrs, Children) ->
    lustre@element:element(<<"data"/utf8>>, Attrs, Children).

-spec dfn(
    list(lustre@internals@vdom:attribute(TIT)),
    list(lustre@internals@vdom:element(TIT))
) -> lustre@internals@vdom:element(TIT).
dfn(Attrs, Children) ->
    lustre@element:element(<<"dfn"/utf8>>, Attrs, Children).

-spec em(
    list(lustre@internals@vdom:attribute(TIZ)),
    list(lustre@internals@vdom:element(TIZ))
) -> lustre@internals@vdom:element(TIZ).
em(Attrs, Children) ->
    lustre@element:element(<<"em"/utf8>>, Attrs, Children).

-spec i(
    list(lustre@internals@vdom:attribute(TJF)),
    list(lustre@internals@vdom:element(TJF))
) -> lustre@internals@vdom:element(TJF).
i(Attrs, Children) ->
    lustre@element:element(<<"i"/utf8>>, Attrs, Children).

-spec kbd(
    list(lustre@internals@vdom:attribute(TJL)),
    list(lustre@internals@vdom:element(TJL))
) -> lustre@internals@vdom:element(TJL).
kbd(Attrs, Children) ->
    lustre@element:element(<<"kbd"/utf8>>, Attrs, Children).

-spec mark(
    list(lustre@internals@vdom:attribute(TJR)),
    list(lustre@internals@vdom:element(TJR))
) -> lustre@internals@vdom:element(TJR).
mark(Attrs, Children) ->
    lustre@element:element(<<"mark"/utf8>>, Attrs, Children).

-spec q(
    list(lustre@internals@vdom:attribute(TJX)),
    list(lustre@internals@vdom:element(TJX))
) -> lustre@internals@vdom:element(TJX).
q(Attrs, Children) ->
    lustre@element:element(<<"q"/utf8>>, Attrs, Children).

-spec rp(
    list(lustre@internals@vdom:attribute(TKD)),
    list(lustre@internals@vdom:element(TKD))
) -> lustre@internals@vdom:element(TKD).
rp(Attrs, Children) ->
    lustre@element:element(<<"rp"/utf8>>, Attrs, Children).

-spec rt(
    list(lustre@internals@vdom:attribute(TKJ)),
    list(lustre@internals@vdom:element(TKJ))
) -> lustre@internals@vdom:element(TKJ).
rt(Attrs, Children) ->
    lustre@element:element(<<"rt"/utf8>>, Attrs, Children).

-spec ruby(
    list(lustre@internals@vdom:attribute(TKP)),
    list(lustre@internals@vdom:element(TKP))
) -> lustre@internals@vdom:element(TKP).
ruby(Attrs, Children) ->
    lustre@element:element(<<"ruby"/utf8>>, Attrs, Children).

-spec s(
    list(lustre@internals@vdom:attribute(TKV)),
    list(lustre@internals@vdom:element(TKV))
) -> lustre@internals@vdom:element(TKV).
s(Attrs, Children) ->
    lustre@element:element(<<"s"/utf8>>, Attrs, Children).

-spec samp(
    list(lustre@internals@vdom:attribute(TLB)),
    list(lustre@internals@vdom:element(TLB))
) -> lustre@internals@vdom:element(TLB).
samp(Attrs, Children) ->
    lustre@element:element(<<"samp"/utf8>>, Attrs, Children).

-spec small(
    list(lustre@internals@vdom:attribute(TLH)),
    list(lustre@internals@vdom:element(TLH))
) -> lustre@internals@vdom:element(TLH).
small(Attrs, Children) ->
    lustre@element:element(<<"small"/utf8>>, Attrs, Children).

-spec span(
    list(lustre@internals@vdom:attribute(TLN)),
    list(lustre@internals@vdom:element(TLN))
) -> lustre@internals@vdom:element(TLN).
span(Attrs, Children) ->
    lustre@element:element(<<"span"/utf8>>, Attrs, Children).

-spec strong(
    list(lustre@internals@vdom:attribute(TLT)),
    list(lustre@internals@vdom:element(TLT))
) -> lustre@internals@vdom:element(TLT).
strong(Attrs, Children) ->
    lustre@element:element(<<"strong"/utf8>>, Attrs, Children).

-spec sub(
    list(lustre@internals@vdom:attribute(TLZ)),
    list(lustre@internals@vdom:element(TLZ))
) -> lustre@internals@vdom:element(TLZ).
sub(Attrs, Children) ->
    lustre@element:element(<<"sub"/utf8>>, Attrs, Children).

-spec sup(
    list(lustre@internals@vdom:attribute(TMF)),
    list(lustre@internals@vdom:element(TMF))
) -> lustre@internals@vdom:element(TMF).
sup(Attrs, Children) ->
    lustre@element:element(<<"sup"/utf8>>, Attrs, Children).

-spec time(
    list(lustre@internals@vdom:attribute(TML)),
    list(lustre@internals@vdom:element(TML))
) -> lustre@internals@vdom:element(TML).
time(Attrs, Children) ->
    lustre@element:element(<<"time"/utf8>>, Attrs, Children).

-spec u(
    list(lustre@internals@vdom:attribute(TMR)),
    list(lustre@internals@vdom:element(TMR))
) -> lustre@internals@vdom:element(TMR).
u(Attrs, Children) ->
    lustre@element:element(<<"u"/utf8>>, Attrs, Children).

-spec var(
    list(lustre@internals@vdom:attribute(TMX)),
    list(lustre@internals@vdom:element(TMX))
) -> lustre@internals@vdom:element(TMX).
var(Attrs, Children) ->
    lustre@element:element(<<"var"/utf8>>, Attrs, Children).

-spec wbr(list(lustre@internals@vdom:attribute(TND))) -> lustre@internals@vdom:element(TND).
wbr(Attrs) ->
    lustre@element:element(<<"wbr"/utf8>>, Attrs, []).

-spec area(list(lustre@internals@vdom:attribute(TNH))) -> lustre@internals@vdom:element(TNH).
area(Attrs) ->
    lustre@element:element(<<"area"/utf8>>, Attrs, []).

-spec audio(
    list(lustre@internals@vdom:attribute(TNL)),
    list(lustre@internals@vdom:element(TNL))
) -> lustre@internals@vdom:element(TNL).
audio(Attrs, Children) ->
    lustre@element:element(<<"audio"/utf8>>, Attrs, Children).

-spec img(list(lustre@internals@vdom:attribute(TNR))) -> lustre@internals@vdom:element(TNR).
img(Attrs) ->
    lustre@element:element(<<"img"/utf8>>, Attrs, []).

-spec map(
    list(lustre@internals@vdom:attribute(TNV)),
    list(lustre@internals@vdom:element(TNV))
) -> lustre@internals@vdom:element(TNV).
map(Attrs, Children) ->
    lustre@element:element(<<"map"/utf8>>, Attrs, Children).

-spec track(list(lustre@internals@vdom:attribute(TOB))) -> lustre@internals@vdom:element(TOB).
track(Attrs) ->
    lustre@element:element(<<"track"/utf8>>, Attrs, []).

-spec video(
    list(lustre@internals@vdom:attribute(TOF)),
    list(lustre@internals@vdom:element(TOF))
) -> lustre@internals@vdom:element(TOF).
video(Attrs, Children) ->
    lustre@element:element(<<"video"/utf8>>, Attrs, Children).

-spec embed(list(lustre@internals@vdom:attribute(TOL))) -> lustre@internals@vdom:element(TOL).
embed(Attrs) ->
    lustre@element:element(<<"embed"/utf8>>, Attrs, []).

-spec iframe(list(lustre@internals@vdom:attribute(TOP))) -> lustre@internals@vdom:element(TOP).
iframe(Attrs) ->
    lustre@element:element(<<"iframe"/utf8>>, Attrs, []).

-spec object(list(lustre@internals@vdom:attribute(TOT))) -> lustre@internals@vdom:element(TOT).
object(Attrs) ->
    lustre@element:element(<<"object"/utf8>>, Attrs, []).

-spec picture(
    list(lustre@internals@vdom:attribute(TOX)),
    list(lustre@internals@vdom:element(TOX))
) -> lustre@internals@vdom:element(TOX).
picture(Attrs, Children) ->
    lustre@element:element(<<"picture"/utf8>>, Attrs, Children).

-spec portal(list(lustre@internals@vdom:attribute(TPD))) -> lustre@internals@vdom:element(TPD).
portal(Attrs) ->
    lustre@element:element(<<"portal"/utf8>>, Attrs, []).

-spec source(list(lustre@internals@vdom:attribute(TPH))) -> lustre@internals@vdom:element(TPH).
source(Attrs) ->
    lustre@element:element(<<"source"/utf8>>, Attrs, []).

-spec svg(
    list(lustre@internals@vdom:attribute(TPL)),
    list(lustre@internals@vdom:element(TPL))
) -> lustre@internals@vdom:element(TPL).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-spec math(
    list(lustre@internals@vdom:attribute(TPR)),
    list(lustre@internals@vdom:element(TPR))
) -> lustre@internals@vdom:element(TPR).
math(Attrs, Children) ->
    lustre@element:element(<<"math"/utf8>>, Attrs, Children).

-spec canvas(list(lustre@internals@vdom:attribute(TPX))) -> lustre@internals@vdom:element(TPX).
canvas(Attrs) ->
    lustre@element:element(<<"canvas"/utf8>>, Attrs, []).

-spec noscript(
    list(lustre@internals@vdom:attribute(TQB)),
    list(lustre@internals@vdom:element(TQB))
) -> lustre@internals@vdom:element(TQB).
noscript(Attrs, Children) ->
    lustre@element:element(<<"noscript"/utf8>>, Attrs, Children).

-spec script(list(lustre@internals@vdom:attribute(TQH)), binary()) -> lustre@internals@vdom:element(TQH).
script(Attrs, Js) ->
    lustre@element:element(<<"script"/utf8>>, Attrs, [text(Js)]).

-spec del(
    list(lustre@internals@vdom:attribute(TQL)),
    list(lustre@internals@vdom:element(TQL))
) -> lustre@internals@vdom:element(TQL).
del(Attrs, Children) ->
    lustre@element:element(<<"del"/utf8>>, Attrs, Children).

-spec ins(
    list(lustre@internals@vdom:attribute(TQR)),
    list(lustre@internals@vdom:element(TQR))
) -> lustre@internals@vdom:element(TQR).
ins(Attrs, Children) ->
    lustre@element:element(<<"ins"/utf8>>, Attrs, Children).

-spec caption(
    list(lustre@internals@vdom:attribute(TQX)),
    list(lustre@internals@vdom:element(TQX))
) -> lustre@internals@vdom:element(TQX).
caption(Attrs, Children) ->
    lustre@element:element(<<"caption"/utf8>>, Attrs, Children).

-spec col(list(lustre@internals@vdom:attribute(TRD))) -> lustre@internals@vdom:element(TRD).
col(Attrs) ->
    lustre@element:element(<<"col"/utf8>>, Attrs, []).

-spec colgroup(
    list(lustre@internals@vdom:attribute(TRH)),
    list(lustre@internals@vdom:element(TRH))
) -> lustre@internals@vdom:element(TRH).
colgroup(Attrs, Children) ->
    lustre@element:element(<<"colgroup"/utf8>>, Attrs, Children).

-spec table(
    list(lustre@internals@vdom:attribute(TRN)),
    list(lustre@internals@vdom:element(TRN))
) -> lustre@internals@vdom:element(TRN).
table(Attrs, Children) ->
    lustre@element:element(<<"table"/utf8>>, Attrs, Children).

-spec tbody(
    list(lustre@internals@vdom:attribute(TRT)),
    list(lustre@internals@vdom:element(TRT))
) -> lustre@internals@vdom:element(TRT).
tbody(Attrs, Children) ->
    lustre@element:element(<<"tbody"/utf8>>, Attrs, Children).

-spec td(
    list(lustre@internals@vdom:attribute(TRZ)),
    list(lustre@internals@vdom:element(TRZ))
) -> lustre@internals@vdom:element(TRZ).
td(Attrs, Children) ->
    lustre@element:element(<<"td"/utf8>>, Attrs, Children).

-spec tfoot(
    list(lustre@internals@vdom:attribute(TSF)),
    list(lustre@internals@vdom:element(TSF))
) -> lustre@internals@vdom:element(TSF).
tfoot(Attrs, Children) ->
    lustre@element:element(<<"tfoot"/utf8>>, Attrs, Children).

-spec th(
    list(lustre@internals@vdom:attribute(TSL)),
    list(lustre@internals@vdom:element(TSL))
) -> lustre@internals@vdom:element(TSL).
th(Attrs, Children) ->
    lustre@element:element(<<"th"/utf8>>, Attrs, Children).

-spec thead(
    list(lustre@internals@vdom:attribute(TSR)),
    list(lustre@internals@vdom:element(TSR))
) -> lustre@internals@vdom:element(TSR).
thead(Attrs, Children) ->
    lustre@element:element(<<"thead"/utf8>>, Attrs, Children).

-spec tr(
    list(lustre@internals@vdom:attribute(TSX)),
    list(lustre@internals@vdom:element(TSX))
) -> lustre@internals@vdom:element(TSX).
tr(Attrs, Children) ->
    lustre@element:element(<<"tr"/utf8>>, Attrs, Children).

-spec button(
    list(lustre@internals@vdom:attribute(TTD)),
    list(lustre@internals@vdom:element(TTD))
) -> lustre@internals@vdom:element(TTD).
button(Attrs, Children) ->
    lustre@element:element(<<"button"/utf8>>, Attrs, Children).

-spec datalist(
    list(lustre@internals@vdom:attribute(TTJ)),
    list(lustre@internals@vdom:element(TTJ))
) -> lustre@internals@vdom:element(TTJ).
datalist(Attrs, Children) ->
    lustre@element:element(<<"datalist"/utf8>>, Attrs, Children).

-spec fieldset(
    list(lustre@internals@vdom:attribute(TTP)),
    list(lustre@internals@vdom:element(TTP))
) -> lustre@internals@vdom:element(TTP).
fieldset(Attrs, Children) ->
    lustre@element:element(<<"fieldset"/utf8>>, Attrs, Children).

-spec form(
    list(lustre@internals@vdom:attribute(TTV)),
    list(lustre@internals@vdom:element(TTV))
) -> lustre@internals@vdom:element(TTV).
form(Attrs, Children) ->
    lustre@element:element(<<"form"/utf8>>, Attrs, Children).

-spec input(list(lustre@internals@vdom:attribute(TUB))) -> lustre@internals@vdom:element(TUB).
input(Attrs) ->
    lustre@element:element(<<"input"/utf8>>, Attrs, []).

-spec label(
    list(lustre@internals@vdom:attribute(TUF)),
    list(lustre@internals@vdom:element(TUF))
) -> lustre@internals@vdom:element(TUF).
label(Attrs, Children) ->
    lustre@element:element(<<"label"/utf8>>, Attrs, Children).

-spec legend(
    list(lustre@internals@vdom:attribute(TUL)),
    list(lustre@internals@vdom:element(TUL))
) -> lustre@internals@vdom:element(TUL).
legend(Attrs, Children) ->
    lustre@element:element(<<"legend"/utf8>>, Attrs, Children).

-spec meter(
    list(lustre@internals@vdom:attribute(TUR)),
    list(lustre@internals@vdom:element(TUR))
) -> lustre@internals@vdom:element(TUR).
meter(Attrs, Children) ->
    lustre@element:element(<<"meter"/utf8>>, Attrs, Children).

-spec optgroup(
    list(lustre@internals@vdom:attribute(TUX)),
    list(lustre@internals@vdom:element(TUX))
) -> lustre@internals@vdom:element(TUX).
optgroup(Attrs, Children) ->
    lustre@element:element(<<"optgroup"/utf8>>, Attrs, Children).

-spec option(list(lustre@internals@vdom:attribute(TVD)), binary()) -> lustre@internals@vdom:element(TVD).
option(Attrs, Label) ->
    lustre@element:element(
        <<"option"/utf8>>,
        Attrs,
        [lustre@element:text(Label)]
    ).

-spec output(
    list(lustre@internals@vdom:attribute(TVH)),
    list(lustre@internals@vdom:element(TVH))
) -> lustre@internals@vdom:element(TVH).
output(Attrs, Children) ->
    lustre@element:element(<<"output"/utf8>>, Attrs, Children).

-spec progress(
    list(lustre@internals@vdom:attribute(TVN)),
    list(lustre@internals@vdom:element(TVN))
) -> lustre@internals@vdom:element(TVN).
progress(Attrs, Children) ->
    lustre@element:element(<<"progress"/utf8>>, Attrs, Children).

-spec select(
    list(lustre@internals@vdom:attribute(TVT)),
    list(lustre@internals@vdom:element(TVT))
) -> lustre@internals@vdom:element(TVT).
select(Attrs, Children) ->
    lustre@element:element(<<"select"/utf8>>, Attrs, Children).

-spec textarea(list(lustre@internals@vdom:attribute(TVZ)), binary()) -> lustre@internals@vdom:element(TVZ).
textarea(Attrs, Content) ->
    lustre@element:element(
        <<"textarea"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-spec details(
    list(lustre@internals@vdom:attribute(TWD)),
    list(lustre@internals@vdom:element(TWD))
) -> lustre@internals@vdom:element(TWD).
details(Attrs, Children) ->
    lustre@element:element(<<"details"/utf8>>, Attrs, Children).

-spec dialog(
    list(lustre@internals@vdom:attribute(TWJ)),
    list(lustre@internals@vdom:element(TWJ))
) -> lustre@internals@vdom:element(TWJ).
dialog(Attrs, Children) ->
    lustre@element:element(<<"dialog"/utf8>>, Attrs, Children).

-spec summary(
    list(lustre@internals@vdom:attribute(TWP)),
    list(lustre@internals@vdom:element(TWP))
) -> lustre@internals@vdom:element(TWP).
summary(Attrs, Children) ->
    lustre@element:element(<<"summary"/utf8>>, Attrs, Children).

-spec slot(list(lustre@internals@vdom:attribute(TWV))) -> lustre@internals@vdom:element(TWV).
slot(Attrs) ->
    lustre@element:element(<<"slot"/utf8>>, Attrs, []).

-spec template(
    list(lustre@internals@vdom:attribute(TWZ)),
    list(lustre@internals@vdom:element(TWZ))
) -> lustre@internals@vdom:element(TWZ).
template(Attrs, Children) ->
    lustre@element:element(<<"template"/utf8>>, Attrs, Children).
