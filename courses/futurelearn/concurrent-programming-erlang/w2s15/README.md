# Supervision

```
$ erl
Erlang/OTP 23 [erts-11.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

Eshell V11.0  (abort with ^G)
1> [c(M) || M <- [echo, talk, super]].
[{ok,echo},{ok,talk},{ok,super}]
2> super:start_link().
{message,#Ref<0.2500056691.802422785.173682>,0} sent.
{message,#Ref<0.2500056691.802422785.173682>,0} echoed.
{ok,<0.96.0>}
{message,#Ref<0.2500056691.802422785.173688>,1} sent.
{message,#Ref<0.2500056691.802422785.173688>,1} echoed.
{message,#Ref<0.2500056691.802422785.173691>,2} sent.
{message,#Ref<0.2500056691.802422785.173691>,2} echoed.
{message,#Ref<0.2500056691.802422785.173694>,3} sent.
{message,#Ref<0.2500056691.802422785.173694>,3} echoed.
{message,#Ref<0.2500056691.802422785.173697>,4} sent.
{message,#Ref<0.2500056691.802422785.173697>,4} echoed.
{message,#Ref<0.2500056691.802422785.173700>,5} sent.
{message,#Ref<0.2500056691.802422785.173700>,5} echoed.
{message,#Ref<0.2500056691.802422785.173703>,6} sent.
{message,#Ref<0.2500056691.802422785.173703>,6} echoed.
3> exit(whereis(echo), kill).
echo worker died: killed. restart now...
true
{message,#Ref<0.2500056691.802422785.173711>,7} sent.
{message,#Ref<0.2500056691.802422785.173711>,7} echoed.
{message,#Ref<0.2500056691.802422785.173714>,8} sent.
{message,#Ref<0.2500056691.802422785.173714>,8} echoed.
{message,#Ref<0.2500056691.802422785.173717>,9} sent.
{message,#Ref<0.2500056691.802422785.173717>,9} echoed.
{message,#Ref<0.2500056691.802422785.173720>,10} sent.
{message,#Ref<0.2500056691.802422785.173720>,10} echoed.
{message,#Ref<0.2500056691.802422785.173723>,11} sent.
{message,#Ref<0.2500056691.802422785.173723>,11} echoed.
4> exit(whereis(talk), kill).
talk worker died: killed. restart now...
true
{message,#Ref<0.2500056691.802422785.173730>,0} sent.
{message,#Ref<0.2500056691.802422785.173730>,0} echoed.
{message,#Ref<0.2500056691.802422785.173734>,1} sent.
{message,#Ref<0.2500056691.802422785.173734>,1} echoed.
{message,#Ref<0.2500056691.802422785.173737>,2} sent.
{message,#Ref<0.2500056691.802422785.173737>,2} echoed.
{message,#Ref<0.2500056691.802422785.173740>,3} sent.
{message,#Ref<0.2500056691.802422785.173740>,3} echoed.
{message,#Ref<0.2500056691.802422785.173743>,4} sent.
{message,#Ref<0.2500056691.802422785.173743>,4} echoed.
{message,#Ref<0.2500056691.802422785.173746>,5} sent.
{message,#Ref<0.2500056691.802422785.173746>,5} echoed.
{message,#Ref<0.2500056691.802422785.173749>,6} sent.
{message,#Ref<0.2500056691.802422785.173749>,6} echoed.
5> exit(whereis(talk), normal).
true
{message,#Ref<0.2500056691.802422785.173756>,7} sent.
{message,#Ref<0.2500056691.802422785.173756>,7} echoed.
{message,#Ref<0.2500056691.802422785.173759>,8} sent.
{message,#Ref<0.2500056691.802422785.173759>,8} echoed.
{message,#Ref<0.2500056691.802422785.173762>,9} sent.
{message,#Ref<0.2500056691.802422785.173762>,9} echoed.
{message,#Ref<0.2500056691.802422785.173765>,10} sent.
{message,#Ref<0.2500056691.802422785.173765>,10} echoed.
{message,#Ref<0.2500056691.802422785.173768>,11} sent.
{message,#Ref<0.2500056691.802422785.173768>,11} echoed.
{message,#Ref<0.2500056691.802422785.173771>,12} sent.
{message,#Ref<0.2500056691.802422785.173771>,12} echoed.
6> 
{message,#Ref<0.2500056691.802422785.173774>,13} sent.
{message,#Ref<0.2500056691.802422785.173774>,13} echoed.
{message,#Ref<0.2500056691.802422785.173777>,14} sent.
{message,#Ref<0.2500056691.802422785.173777>,14} echoed.
6> echo ! {exit, normal}.
exit talk with reason normal.
{exit,normal}
echo worker died: normal. restart now...
{message,#Ref<0.2500056691.802422785.173786>,15} sent.
{message,#Ref<0.2500056691.802422785.173786>,15} echoed.
{message,#Ref<0.2500056691.802422785.173789>,16} sent.
{message,#Ref<0.2500056691.802422785.173789>,16} echoed.
{message,#Ref<0.2500056691.802422785.173792>,17} sent.
{message,#Ref<0.2500056691.802422785.173792>,17} echoed.
{message,#Ref<0.2500056691.802422785.173795>,18} sent.
{message,#Ref<0.2500056691.802422785.173795>,18} echoed.
{message,#Ref<0.2500056691.802422785.173798>,19} sent.
{message,#Ref<0.2500056691.802422785.173798>,19} echoed.
{message,#Ref<0.2500056691.802422785.173801>,20} sent.
{message,#Ref<0.2500056691.802422785.173801>,20} echoed.
7> echo ! {exit, kill}.
exit talk with reason kill.
{exit,kill}
echo worker died: kill. restart now...
{message,#Ref<0.2500056691.802422785.173810>,21} sent.
{message,#Ref<0.2500056691.802422785.173810>,21} echoed.
{message,#Ref<0.2500056691.802422785.173813>,22} sent.
{message,#Ref<0.2500056691.802422785.173813>,22} echoed.
{message,#Ref<0.2500056691.802422785.173816>,23} sent.
{message,#Ref<0.2500056691.802422785.173816>,23} echoed.
{message,#Ref<0.2500056691.802422785.173819>,24} sent.
{message,#Ref<0.2500056691.802422785.173819>,24} echoed.
{message,#Ref<0.2500056691.802422785.173822>,25} sent.
{message,#Ref<0.2500056691.802422785.173822>,25} echoed.
{message,#Ref<0.2500056691.802422785.173825>,26} sent.
{message,#Ref<0.2500056691.802422785.173825>,26} echoed.
8> echo ! {exit, xpto}.
exit talk with reason xpto.
{exit,xpto}
echo worker died: xpto. restart now...
{message,#Ref<0.2500056691.802422785.173834>,27} sent.
{message,#Ref<0.2500056691.802422785.173834>,27} echoed.
{message,#Ref<0.2500056691.802422785.173837>,28} sent.
{message,#Ref<0.2500056691.802422785.173837>,28} echoed.
{message,#Ref<0.2500056691.802422785.173840>,29} sent.
{message,#Ref<0.2500056691.802422785.173840>,29} echoed.
{message,#Ref<0.2500056691.802422785.173843>,30} sent.
{message,#Ref<0.2500056691.802422785.173843>,30} echoed.
{message,#Ref<0.2500056691.802422785.173846>,31} sent.
{message,#Ref<0.2500056691.802422785.173846>,31} echoed.
{message,#Ref<0.2500056691.802422785.173849>,32} sent.
{message,#Ref<0.2500056691.802422785.173849>,32} echoed.
9> super:stop().
stopped echo worker.
ok
10> =ERROR REPORT==== 4-Jul-2020::13:22:46.428865 ===
Error in process <0.102.0> with exit value:
{badarg,[{echo,handle_cast,2,[{file,"echo.erl"},{line,58}]},
         {talk,loop,1,[{file,"talk.erl"},{line,65}]}]}

```