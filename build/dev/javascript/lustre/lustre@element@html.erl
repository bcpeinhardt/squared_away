-module(lustre@element@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([html/2, text/1, base/1, head/2, link/1, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, hgroup/2, main/2, nav/2, section/2, search/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, data/2, dfn/2, em/2, i/2, kbd/2, mark/2, q/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var/2, wbr/1, area/1, audio/2, img/1, map/2, track/1, video/2, embed/1, iframe/1, object/1, picture/2, portal/1, source/1, svg/2, math/2, canvas/1, noscript/2, script/2, del/2, ins/2, caption/2, col/1, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/2, output/2, progress/2, select/2, textarea/2, details/2, dialog/2, summary/2, slot/1, template/2]).

-spec html(
    list(lustre@internals@vdom:attribute(PVZ)),
    list(lustre@internals@vdom:element(PVZ))
) -> lustre@internals@vdom:element(PVZ).
html(Attrs, Children) ->
    lustre@element:element(<<"html"/utf8>>, Attrs, Children).

-spec text(binary()) -> lustre@internals@vdom:element(any()).
text(Content) ->
    lustre@element:text(Content).

-spec base(list(lustre@internals@vdom:attribute(PWH))) -> lustre@internals@vdom:element(PWH).
base(Attrs) ->
    lustre@element:element(<<"base"/utf8>>, Attrs, []).

-spec head(
    list(lustre@internals@vdom:attribute(PWL)),
    list(lustre@internals@vdom:element(PWL))
) -> lustre@internals@vdom:element(PWL).
head(Attrs, Children) ->
    lustre@element:element(<<"head"/utf8>>, Attrs, Children).

-spec link(list(lustre@internals@vdom:attribute(PWR))) -> lustre@internals@vdom:element(PWR).
link(Attrs) ->
    lustre@element:element(<<"link"/utf8>>, Attrs, []).

-spec meta(list(lustre@internals@vdom:attribute(PWV))) -> lustre@internals@vdom:element(PWV).
meta(Attrs) ->
    lustre@element:element(<<"meta"/utf8>>, Attrs, []).

-spec style(list(lustre@internals@vdom:attribute(PWZ)), binary()) -> lustre@internals@vdom:element(PWZ).
style(Attrs, Css) ->
    lustre@element:element(<<"style"/utf8>>, Attrs, [text(Css)]).

-spec title(list(lustre@internals@vdom:attribute(PXD)), binary()) -> lustre@internals@vdom:element(PXD).
title(Attrs, Content) ->
    lustre@element:element(<<"title"/utf8>>, Attrs, [text(Content)]).

-spec body(
    list(lustre@internals@vdom:attribute(PXH)),
    list(lustre@internals@vdom:element(PXH))
) -> lustre@internals@vdom:element(PXH).
body(Attrs, Children) ->
    lustre@element:element(<<"body"/utf8>>, Attrs, Children).

-spec address(
    list(lustre@internals@vdom:attribute(PXN)),
    list(lustre@internals@vdom:element(PXN))
) -> lustre@internals@vdom:element(PXN).
address(Attrs, Children) ->
    lustre@element:element(<<"address"/utf8>>, Attrs, Children).

-spec article(
    list(lustre@internals@vdom:attribute(PXT)),
    list(lustre@internals@vdom:element(PXT))
) -> lustre@internals@vdom:element(PXT).
article(Attrs, Children) ->
    lustre@element:element(<<"article"/utf8>>, Attrs, Children).

-spec aside(
    list(lustre@internals@vdom:attribute(PXZ)),
    list(lustre@internals@vdom:element(PXZ))
) -> lustre@internals@vdom:element(PXZ).
aside(Attrs, Children) ->
    lustre@element:element(<<"aside"/utf8>>, Attrs, Children).

-spec footer(
    list(lustre@internals@vdom:attribute(PYF)),
    list(lustre@internals@vdom:element(PYF))
) -> lustre@internals@vdom:element(PYF).
footer(Attrs, Children) ->
    lustre@element:element(<<"footer"/utf8>>, Attrs, Children).

-spec header(
    list(lustre@internals@vdom:attribute(PYL)),
    list(lustre@internals@vdom:element(PYL))
) -> lustre@internals@vdom:element(PYL).
header(Attrs, Children) ->
    lustre@element:element(<<"header"/utf8>>, Attrs, Children).

-spec h1(
    list(lustre@internals@vdom:attribute(PYR)),
    list(lustre@internals@vdom:element(PYR))
) -> lustre@internals@vdom:element(PYR).
h1(Attrs, Children) ->
    lustre@element:element(<<"h1"/utf8>>, Attrs, Children).

-spec h2(
    list(lustre@internals@vdom:attribute(PYX)),
    list(lustre@internals@vdom:element(PYX))
) -> lustre@internals@vdom:element(PYX).
h2(Attrs, Children) ->
    lustre@element:element(<<"h2"/utf8>>, Attrs, Children).

-spec h3(
    list(lustre@internals@vdom:attribute(PZD)),
    list(lustre@internals@vdom:element(PZD))
) -> lustre@internals@vdom:element(PZD).
h3(Attrs, Children) ->
    lustre@element:element(<<"h3"/utf8>>, Attrs, Children).

-spec h4(
    list(lustre@internals@vdom:attribute(PZJ)),
    list(lustre@internals@vdom:element(PZJ))
) -> lustre@internals@vdom:element(PZJ).
h4(Attrs, Children) ->
    lustre@element:element(<<"h4"/utf8>>, Attrs, Children).

-spec h5(
    list(lustre@internals@vdom:attribute(PZP)),
    list(lustre@internals@vdom:element(PZP))
) -> lustre@internals@vdom:element(PZP).
h5(Attrs, Children) ->
    lustre@element:element(<<"h5"/utf8>>, Attrs, Children).

-spec h6(
    list(lustre@internals@vdom:attribute(PZV)),
    list(lustre@internals@vdom:element(PZV))
) -> lustre@internals@vdom:element(PZV).
h6(Attrs, Children) ->
    lustre@element:element(<<"h6"/utf8>>, Attrs, Children).

-spec hgroup(
    list(lustre@internals@vdom:attribute(QAB)),
    list(lustre@internals@vdom:element(QAB))
) -> lustre@internals@vdom:element(QAB).
hgroup(Attrs, Children) ->
    lustre@element:element(<<"hgroup"/utf8>>, Attrs, Children).

-spec main(
    list(lustre@internals@vdom:attribute(QAH)),
    list(lustre@internals@vdom:element(QAH))
) -> lustre@internals@vdom:element(QAH).
main(Attrs, Children) ->
    lustre@element:element(<<"main"/utf8>>, Attrs, Children).

-spec nav(
    list(lustre@internals@vdom:attribute(QAN)),
    list(lustre@internals@vdom:element(QAN))
) -> lustre@internals@vdom:element(QAN).
nav(Attrs, Children) ->
    lustre@element:element(<<"nav"/utf8>>, Attrs, Children).

-spec section(
    list(lustre@internals@vdom:attribute(QAT)),
    list(lustre@internals@vdom:element(QAT))
) -> lustre@internals@vdom:element(QAT).
section(Attrs, Children) ->
    lustre@element:element(<<"section"/utf8>>, Attrs, Children).

-spec search(
    list(lustre@internals@vdom:attribute(QAZ)),
    list(lustre@internals@vdom:element(QAZ))
) -> lustre@internals@vdom:element(QAZ).
search(Attrs, Children) ->
    lustre@element:element(<<"search"/utf8>>, Attrs, Children).

-spec blockquote(
    list(lustre@internals@vdom:attribute(QBF)),
    list(lustre@internals@vdom:element(QBF))
) -> lustre@internals@vdom:element(QBF).
blockquote(Attrs, Children) ->
    lustre@element:element(<<"blockquote"/utf8>>, Attrs, Children).

-spec dd(
    list(lustre@internals@vdom:attribute(QBL)),
    list(lustre@internals@vdom:element(QBL))
) -> lustre@internals@vdom:element(QBL).
dd(Attrs, Children) ->
    lustre@element:element(<<"dd"/utf8>>, Attrs, Children).

-spec 'div'(
    list(lustre@internals@vdom:attribute(QBR)),
    list(lustre@internals@vdom:element(QBR))
) -> lustre@internals@vdom:element(QBR).
'div'(Attrs, Children) ->
    lustre@element:element(<<"div"/utf8>>, Attrs, Children).

-spec dl(
    list(lustre@internals@vdom:attribute(QBX)),
    list(lustre@internals@vdom:element(QBX))
) -> lustre@internals@vdom:element(QBX).
dl(Attrs, Children) ->
    lustre@element:element(<<"dl"/utf8>>, Attrs, Children).

-spec dt(
    list(lustre@internals@vdom:attribute(QCD)),
    list(lustre@internals@vdom:element(QCD))
) -> lustre@internals@vdom:element(QCD).
dt(Attrs, Children) ->
    lustre@element:element(<<"dt"/utf8>>, Attrs, Children).

-spec figcaption(
    list(lustre@internals@vdom:attribute(QCJ)),
    list(lustre@internals@vdom:element(QCJ))
) -> lustre@internals@vdom:element(QCJ).
figcaption(Attrs, Children) ->
    lustre@element:element(<<"figcaption"/utf8>>, Attrs, Children).

-spec figure(
    list(lustre@internals@vdom:attribute(QCP)),
    list(lustre@internals@vdom:element(QCP))
) -> lustre@internals@vdom:element(QCP).
figure(Attrs, Children) ->
    lustre@element:element(<<"figure"/utf8>>, Attrs, Children).

-spec hr(list(lustre@internals@vdom:attribute(QCV))) -> lustre@internals@vdom:element(QCV).
hr(Attrs) ->
    lustre@element:element(<<"hr"/utf8>>, Attrs, []).

-spec li(
    list(lustre@internals@vdom:attribute(QCZ)),
    list(lustre@internals@vdom:element(QCZ))
) -> lustre@internals@vdom:element(QCZ).
li(Attrs, Children) ->
    lustre@element:element(<<"li"/utf8>>, Attrs, Children).

-spec menu(
    list(lustre@internals@vdom:attribute(QDF)),
    list(lustre@internals@vdom:element(QDF))
) -> lustre@internals@vdom:element(QDF).
menu(Attrs, Children) ->
    lustre@element:element(<<"menu"/utf8>>, Attrs, Children).

-spec ol(
    list(lustre@internals@vdom:attribute(QDL)),
    list(lustre@internals@vdom:element(QDL))
) -> lustre@internals@vdom:element(QDL).
ol(Attrs, Children) ->
    lustre@element:element(<<"ol"/utf8>>, Attrs, Children).

-spec p(
    list(lustre@internals@vdom:attribute(QDR)),
    list(lustre@internals@vdom:element(QDR))
) -> lustre@internals@vdom:element(QDR).
p(Attrs, Children) ->
    lustre@element:element(<<"p"/utf8>>, Attrs, Children).

-spec pre(
    list(lustre@internals@vdom:attribute(QDX)),
    list(lustre@internals@vdom:element(QDX))
) -> lustre@internals@vdom:element(QDX).
pre(Attrs, Children) ->
    lustre@element:element(<<"pre"/utf8>>, Attrs, Children).

-spec ul(
    list(lustre@internals@vdom:attribute(QED)),
    list(lustre@internals@vdom:element(QED))
) -> lustre@internals@vdom:element(QED).
ul(Attrs, Children) ->
    lustre@element:element(<<"ul"/utf8>>, Attrs, Children).

-spec a(
    list(lustre@internals@vdom:attribute(QEJ)),
    list(lustre@internals@vdom:element(QEJ))
) -> lustre@internals@vdom:element(QEJ).
a(Attrs, Children) ->
    lustre@element:element(<<"a"/utf8>>, Attrs, Children).

-spec abbr(
    list(lustre@internals@vdom:attribute(QEP)),
    list(lustre@internals@vdom:element(QEP))
) -> lustre@internals@vdom:element(QEP).
abbr(Attrs, Children) ->
    lustre@element:element(<<"abbr"/utf8>>, Attrs, Children).

-spec b(
    list(lustre@internals@vdom:attribute(QEV)),
    list(lustre@internals@vdom:element(QEV))
) -> lustre@internals@vdom:element(QEV).
b(Attrs, Children) ->
    lustre@element:element(<<"b"/utf8>>, Attrs, Children).

-spec bdi(
    list(lustre@internals@vdom:attribute(QFB)),
    list(lustre@internals@vdom:element(QFB))
) -> lustre@internals@vdom:element(QFB).
bdi(Attrs, Children) ->
    lustre@element:element(<<"bdi"/utf8>>, Attrs, Children).

-spec bdo(
    list(lustre@internals@vdom:attribute(QFH)),
    list(lustre@internals@vdom:element(QFH))
) -> lustre@internals@vdom:element(QFH).
bdo(Attrs, Children) ->
    lustre@element:element(<<"bdo"/utf8>>, Attrs, Children).

-spec br(list(lustre@internals@vdom:attribute(QFN))) -> lustre@internals@vdom:element(QFN).
br(Attrs) ->
    lustre@element:element(<<"br"/utf8>>, Attrs, []).

-spec cite(
    list(lustre@internals@vdom:attribute(QFR)),
    list(lustre@internals@vdom:element(QFR))
) -> lustre@internals@vdom:element(QFR).
cite(Attrs, Children) ->
    lustre@element:element(<<"cite"/utf8>>, Attrs, Children).

-spec code(
    list(lustre@internals@vdom:attribute(QFX)),
    list(lustre@internals@vdom:element(QFX))
) -> lustre@internals@vdom:element(QFX).
code(Attrs, Children) ->
    lustre@element:element(<<"code"/utf8>>, Attrs, Children).

-spec data(
    list(lustre@internals@vdom:attribute(QGD)),
    list(lustre@internals@vdom:element(QGD))
) -> lustre@internals@vdom:element(QGD).
data(Attrs, Children) ->
    lustre@element:element(<<"data"/utf8>>, Attrs, Children).

-spec dfn(
    list(lustre@internals@vdom:attribute(QGJ)),
    list(lustre@internals@vdom:element(QGJ))
) -> lustre@internals@vdom:element(QGJ).
dfn(Attrs, Children) ->
    lustre@element:element(<<"dfn"/utf8>>, Attrs, Children).

-spec em(
    list(lustre@internals@vdom:attribute(QGP)),
    list(lustre@internals@vdom:element(QGP))
) -> lustre@internals@vdom:element(QGP).
em(Attrs, Children) ->
    lustre@element:element(<<"em"/utf8>>, Attrs, Children).

-spec i(
    list(lustre@internals@vdom:attribute(QGV)),
    list(lustre@internals@vdom:element(QGV))
) -> lustre@internals@vdom:element(QGV).
i(Attrs, Children) ->
    lustre@element:element(<<"i"/utf8>>, Attrs, Children).

-spec kbd(
    list(lustre@internals@vdom:attribute(QHB)),
    list(lustre@internals@vdom:element(QHB))
) -> lustre@internals@vdom:element(QHB).
kbd(Attrs, Children) ->
    lustre@element:element(<<"kbd"/utf8>>, Attrs, Children).

-spec mark(
    list(lustre@internals@vdom:attribute(QHH)),
    list(lustre@internals@vdom:element(QHH))
) -> lustre@internals@vdom:element(QHH).
mark(Attrs, Children) ->
    lustre@element:element(<<"mark"/utf8>>, Attrs, Children).

-spec q(
    list(lustre@internals@vdom:attribute(QHN)),
    list(lustre@internals@vdom:element(QHN))
) -> lustre@internals@vdom:element(QHN).
q(Attrs, Children) ->
    lustre@element:element(<<"q"/utf8>>, Attrs, Children).

-spec rp(
    list(lustre@internals@vdom:attribute(QHT)),
    list(lustre@internals@vdom:element(QHT))
) -> lustre@internals@vdom:element(QHT).
rp(Attrs, Children) ->
    lustre@element:element(<<"rp"/utf8>>, Attrs, Children).

-spec rt(
    list(lustre@internals@vdom:attribute(QHZ)),
    list(lustre@internals@vdom:element(QHZ))
) -> lustre@internals@vdom:element(QHZ).
rt(Attrs, Children) ->
    lustre@element:element(<<"rt"/utf8>>, Attrs, Children).

-spec ruby(
    list(lustre@internals@vdom:attribute(QIF)),
    list(lustre@internals@vdom:element(QIF))
) -> lustre@internals@vdom:element(QIF).
ruby(Attrs, Children) ->
    lustre@element:element(<<"ruby"/utf8>>, Attrs, Children).

-spec s(
    list(lustre@internals@vdom:attribute(QIL)),
    list(lustre@internals@vdom:element(QIL))
) -> lustre@internals@vdom:element(QIL).
s(Attrs, Children) ->
    lustre@element:element(<<"s"/utf8>>, Attrs, Children).

-spec samp(
    list(lustre@internals@vdom:attribute(QIR)),
    list(lustre@internals@vdom:element(QIR))
) -> lustre@internals@vdom:element(QIR).
samp(Attrs, Children) ->
    lustre@element:element(<<"samp"/utf8>>, Attrs, Children).

-spec small(
    list(lustre@internals@vdom:attribute(QIX)),
    list(lustre@internals@vdom:element(QIX))
) -> lustre@internals@vdom:element(QIX).
small(Attrs, Children) ->
    lustre@element:element(<<"small"/utf8>>, Attrs, Children).

-spec span(
    list(lustre@internals@vdom:attribute(QJD)),
    list(lustre@internals@vdom:element(QJD))
) -> lustre@internals@vdom:element(QJD).
span(Attrs, Children) ->
    lustre@element:element(<<"span"/utf8>>, Attrs, Children).

-spec strong(
    list(lustre@internals@vdom:attribute(QJJ)),
    list(lustre@internals@vdom:element(QJJ))
) -> lustre@internals@vdom:element(QJJ).
strong(Attrs, Children) ->
    lustre@element:element(<<"strong"/utf8>>, Attrs, Children).

-spec sub(
    list(lustre@internals@vdom:attribute(QJP)),
    list(lustre@internals@vdom:element(QJP))
) -> lustre@internals@vdom:element(QJP).
sub(Attrs, Children) ->
    lustre@element:element(<<"sub"/utf8>>, Attrs, Children).

-spec sup(
    list(lustre@internals@vdom:attribute(QJV)),
    list(lustre@internals@vdom:element(QJV))
) -> lustre@internals@vdom:element(QJV).
sup(Attrs, Children) ->
    lustre@element:element(<<"sup"/utf8>>, Attrs, Children).

-spec time(
    list(lustre@internals@vdom:attribute(QKB)),
    list(lustre@internals@vdom:element(QKB))
) -> lustre@internals@vdom:element(QKB).
time(Attrs, Children) ->
    lustre@element:element(<<"time"/utf8>>, Attrs, Children).

-spec u(
    list(lustre@internals@vdom:attribute(QKH)),
    list(lustre@internals@vdom:element(QKH))
) -> lustre@internals@vdom:element(QKH).
u(Attrs, Children) ->
    lustre@element:element(<<"u"/utf8>>, Attrs, Children).

-spec var(
    list(lustre@internals@vdom:attribute(QKN)),
    list(lustre@internals@vdom:element(QKN))
) -> lustre@internals@vdom:element(QKN).
var(Attrs, Children) ->
    lustre@element:element(<<"var"/utf8>>, Attrs, Children).

-spec wbr(list(lustre@internals@vdom:attribute(QKT))) -> lustre@internals@vdom:element(QKT).
wbr(Attrs) ->
    lustre@element:element(<<"wbr"/utf8>>, Attrs, []).

-spec area(list(lustre@internals@vdom:attribute(QKX))) -> lustre@internals@vdom:element(QKX).
area(Attrs) ->
    lustre@element:element(<<"area"/utf8>>, Attrs, []).

-spec audio(
    list(lustre@internals@vdom:attribute(QLB)),
    list(lustre@internals@vdom:element(QLB))
) -> lustre@internals@vdom:element(QLB).
audio(Attrs, Children) ->
    lustre@element:element(<<"audio"/utf8>>, Attrs, Children).

-spec img(list(lustre@internals@vdom:attribute(QLH))) -> lustre@internals@vdom:element(QLH).
img(Attrs) ->
    lustre@element:element(<<"img"/utf8>>, Attrs, []).

-spec map(
    list(lustre@internals@vdom:attribute(QLL)),
    list(lustre@internals@vdom:element(QLL))
) -> lustre@internals@vdom:element(QLL).
map(Attrs, Children) ->
    lustre@element:element(<<"map"/utf8>>, Attrs, Children).

-spec track(list(lustre@internals@vdom:attribute(QLR))) -> lustre@internals@vdom:element(QLR).
track(Attrs) ->
    lustre@element:element(<<"track"/utf8>>, Attrs, []).

-spec video(
    list(lustre@internals@vdom:attribute(QLV)),
    list(lustre@internals@vdom:element(QLV))
) -> lustre@internals@vdom:element(QLV).
video(Attrs, Children) ->
    lustre@element:element(<<"video"/utf8>>, Attrs, Children).

-spec embed(list(lustre@internals@vdom:attribute(QMB))) -> lustre@internals@vdom:element(QMB).
embed(Attrs) ->
    lustre@element:element(<<"embed"/utf8>>, Attrs, []).

-spec iframe(list(lustre@internals@vdom:attribute(QMF))) -> lustre@internals@vdom:element(QMF).
iframe(Attrs) ->
    lustre@element:element(<<"iframe"/utf8>>, Attrs, []).

-spec object(list(lustre@internals@vdom:attribute(QMJ))) -> lustre@internals@vdom:element(QMJ).
object(Attrs) ->
    lustre@element:element(<<"object"/utf8>>, Attrs, []).

-spec picture(
    list(lustre@internals@vdom:attribute(QMN)),
    list(lustre@internals@vdom:element(QMN))
) -> lustre@internals@vdom:element(QMN).
picture(Attrs, Children) ->
    lustre@element:element(<<"picture"/utf8>>, Attrs, Children).

-spec portal(list(lustre@internals@vdom:attribute(QMT))) -> lustre@internals@vdom:element(QMT).
portal(Attrs) ->
    lustre@element:element(<<"portal"/utf8>>, Attrs, []).

-spec source(list(lustre@internals@vdom:attribute(QMX))) -> lustre@internals@vdom:element(QMX).
source(Attrs) ->
    lustre@element:element(<<"source"/utf8>>, Attrs, []).

-spec svg(
    list(lustre@internals@vdom:attribute(QNB)),
    list(lustre@internals@vdom:element(QNB))
) -> lustre@internals@vdom:element(QNB).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-spec math(
    list(lustre@internals@vdom:attribute(QNH)),
    list(lustre@internals@vdom:element(QNH))
) -> lustre@internals@vdom:element(QNH).
math(Attrs, Children) ->
    lustre@element:element(<<"math"/utf8>>, Attrs, Children).

-spec canvas(list(lustre@internals@vdom:attribute(QNN))) -> lustre@internals@vdom:element(QNN).
canvas(Attrs) ->
    lustre@element:element(<<"canvas"/utf8>>, Attrs, []).

-spec noscript(
    list(lustre@internals@vdom:attribute(QNR)),
    list(lustre@internals@vdom:element(QNR))
) -> lustre@internals@vdom:element(QNR).
noscript(Attrs, Children) ->
    lustre@element:element(<<"noscript"/utf8>>, Attrs, Children).

-spec script(list(lustre@internals@vdom:attribute(QNX)), binary()) -> lustre@internals@vdom:element(QNX).
script(Attrs, Js) ->
    lustre@element:element(<<"script"/utf8>>, Attrs, [text(Js)]).

-spec del(
    list(lustre@internals@vdom:attribute(QOB)),
    list(lustre@internals@vdom:element(QOB))
) -> lustre@internals@vdom:element(QOB).
del(Attrs, Children) ->
    lustre@element:element(<<"del"/utf8>>, Attrs, Children).

-spec ins(
    list(lustre@internals@vdom:attribute(QOH)),
    list(lustre@internals@vdom:element(QOH))
) -> lustre@internals@vdom:element(QOH).
ins(Attrs, Children) ->
    lustre@element:element(<<"ins"/utf8>>, Attrs, Children).

-spec caption(
    list(lustre@internals@vdom:attribute(QON)),
    list(lustre@internals@vdom:element(QON))
) -> lustre@internals@vdom:element(QON).
caption(Attrs, Children) ->
    lustre@element:element(<<"caption"/utf8>>, Attrs, Children).

-spec col(list(lustre@internals@vdom:attribute(QOT))) -> lustre@internals@vdom:element(QOT).
col(Attrs) ->
    lustre@element:element(<<"col"/utf8>>, Attrs, []).

-spec colgroup(
    list(lustre@internals@vdom:attribute(QOX)),
    list(lustre@internals@vdom:element(QOX))
) -> lustre@internals@vdom:element(QOX).
colgroup(Attrs, Children) ->
    lustre@element:element(<<"colgroup"/utf8>>, Attrs, Children).

-spec table(
    list(lustre@internals@vdom:attribute(QPD)),
    list(lustre@internals@vdom:element(QPD))
) -> lustre@internals@vdom:element(QPD).
table(Attrs, Children) ->
    lustre@element:element(<<"table"/utf8>>, Attrs, Children).

-spec tbody(
    list(lustre@internals@vdom:attribute(QPJ)),
    list(lustre@internals@vdom:element(QPJ))
) -> lustre@internals@vdom:element(QPJ).
tbody(Attrs, Children) ->
    lustre@element:element(<<"tbody"/utf8>>, Attrs, Children).

-spec td(
    list(lustre@internals@vdom:attribute(QPP)),
    list(lustre@internals@vdom:element(QPP))
) -> lustre@internals@vdom:element(QPP).
td(Attrs, Children) ->
    lustre@element:element(<<"td"/utf8>>, Attrs, Children).

-spec tfoot(
    list(lustre@internals@vdom:attribute(QPV)),
    list(lustre@internals@vdom:element(QPV))
) -> lustre@internals@vdom:element(QPV).
tfoot(Attrs, Children) ->
    lustre@element:element(<<"tfoot"/utf8>>, Attrs, Children).

-spec th(
    list(lustre@internals@vdom:attribute(QQB)),
    list(lustre@internals@vdom:element(QQB))
) -> lustre@internals@vdom:element(QQB).
th(Attrs, Children) ->
    lustre@element:element(<<"th"/utf8>>, Attrs, Children).

-spec thead(
    list(lustre@internals@vdom:attribute(QQH)),
    list(lustre@internals@vdom:element(QQH))
) -> lustre@internals@vdom:element(QQH).
thead(Attrs, Children) ->
    lustre@element:element(<<"thead"/utf8>>, Attrs, Children).

-spec tr(
    list(lustre@internals@vdom:attribute(QQN)),
    list(lustre@internals@vdom:element(QQN))
) -> lustre@internals@vdom:element(QQN).
tr(Attrs, Children) ->
    lustre@element:element(<<"tr"/utf8>>, Attrs, Children).

-spec button(
    list(lustre@internals@vdom:attribute(QQT)),
    list(lustre@internals@vdom:element(QQT))
) -> lustre@internals@vdom:element(QQT).
button(Attrs, Children) ->
    lustre@element:element(<<"button"/utf8>>, Attrs, Children).

-spec datalist(
    list(lustre@internals@vdom:attribute(QQZ)),
    list(lustre@internals@vdom:element(QQZ))
) -> lustre@internals@vdom:element(QQZ).
datalist(Attrs, Children) ->
    lustre@element:element(<<"datalist"/utf8>>, Attrs, Children).

-spec fieldset(
    list(lustre@internals@vdom:attribute(QRF)),
    list(lustre@internals@vdom:element(QRF))
) -> lustre@internals@vdom:element(QRF).
fieldset(Attrs, Children) ->
    lustre@element:element(<<"fieldset"/utf8>>, Attrs, Children).

-spec form(
    list(lustre@internals@vdom:attribute(QRL)),
    list(lustre@internals@vdom:element(QRL))
) -> lustre@internals@vdom:element(QRL).
form(Attrs, Children) ->
    lustre@element:element(<<"form"/utf8>>, Attrs, Children).

-spec input(list(lustre@internals@vdom:attribute(QRR))) -> lustre@internals@vdom:element(QRR).
input(Attrs) ->
    lustre@element:element(<<"input"/utf8>>, Attrs, []).

-spec label(
    list(lustre@internals@vdom:attribute(QRV)),
    list(lustre@internals@vdom:element(QRV))
) -> lustre@internals@vdom:element(QRV).
label(Attrs, Children) ->
    lustre@element:element(<<"label"/utf8>>, Attrs, Children).

-spec legend(
    list(lustre@internals@vdom:attribute(QSB)),
    list(lustre@internals@vdom:element(QSB))
) -> lustre@internals@vdom:element(QSB).
legend(Attrs, Children) ->
    lustre@element:element(<<"legend"/utf8>>, Attrs, Children).

-spec meter(
    list(lustre@internals@vdom:attribute(QSH)),
    list(lustre@internals@vdom:element(QSH))
) -> lustre@internals@vdom:element(QSH).
meter(Attrs, Children) ->
    lustre@element:element(<<"meter"/utf8>>, Attrs, Children).

-spec optgroup(
    list(lustre@internals@vdom:attribute(QSN)),
    list(lustre@internals@vdom:element(QSN))
) -> lustre@internals@vdom:element(QSN).
optgroup(Attrs, Children) ->
    lustre@element:element(<<"optgroup"/utf8>>, Attrs, Children).

-spec option(list(lustre@internals@vdom:attribute(QST)), binary()) -> lustre@internals@vdom:element(QST).
option(Attrs, Label) ->
    lustre@element:element(
        <<"option"/utf8>>,
        Attrs,
        [lustre@element:text(Label)]
    ).

-spec output(
    list(lustre@internals@vdom:attribute(QSX)),
    list(lustre@internals@vdom:element(QSX))
) -> lustre@internals@vdom:element(QSX).
output(Attrs, Children) ->
    lustre@element:element(<<"output"/utf8>>, Attrs, Children).

-spec progress(
    list(lustre@internals@vdom:attribute(QTD)),
    list(lustre@internals@vdom:element(QTD))
) -> lustre@internals@vdom:element(QTD).
progress(Attrs, Children) ->
    lustre@element:element(<<"progress"/utf8>>, Attrs, Children).

-spec select(
    list(lustre@internals@vdom:attribute(QTJ)),
    list(lustre@internals@vdom:element(QTJ))
) -> lustre@internals@vdom:element(QTJ).
select(Attrs, Children) ->
    lustre@element:element(<<"select"/utf8>>, Attrs, Children).

-spec textarea(list(lustre@internals@vdom:attribute(QTP)), binary()) -> lustre@internals@vdom:element(QTP).
textarea(Attrs, Content) ->
    lustre@element:element(
        <<"textarea"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-spec details(
    list(lustre@internals@vdom:attribute(QTT)),
    list(lustre@internals@vdom:element(QTT))
) -> lustre@internals@vdom:element(QTT).
details(Attrs, Children) ->
    lustre@element:element(<<"details"/utf8>>, Attrs, Children).

-spec dialog(
    list(lustre@internals@vdom:attribute(QTZ)),
    list(lustre@internals@vdom:element(QTZ))
) -> lustre@internals@vdom:element(QTZ).
dialog(Attrs, Children) ->
    lustre@element:element(<<"dialog"/utf8>>, Attrs, Children).

-spec summary(
    list(lustre@internals@vdom:attribute(QUF)),
    list(lustre@internals@vdom:element(QUF))
) -> lustre@internals@vdom:element(QUF).
summary(Attrs, Children) ->
    lustre@element:element(<<"summary"/utf8>>, Attrs, Children).

-spec slot(list(lustre@internals@vdom:attribute(QUL))) -> lustre@internals@vdom:element(QUL).
slot(Attrs) ->
    lustre@element:element(<<"slot"/utf8>>, Attrs, []).

-spec template(
    list(lustre@internals@vdom:attribute(QUP)),
    list(lustre@internals@vdom:element(QUP))
) -> lustre@internals@vdom:element(QUP).
template(Attrs, Children) ->
    lustre@element:element(<<"template"/utf8>>, Attrs, Children).
