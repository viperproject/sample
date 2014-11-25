
import ch.ethz.inf.pm.td
import ch.ethz.inf.pm.td.analysis.TestRunner

val toAnalyze =
  """#inyha
    |#epppa
    |#hdfdb
    |#wsae
    |#kavs
    |#ayrma
    |#kzolclas
    |#kzolclas
    |nhbr
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |wsae
    |ehkdudjl
    |vnnnb
    |yadvb
    |bety
    |unjm
    |lngca
    |gmwergth
    |bmlqa
    |hktu
    |jjmq
    |vdcc
    |fzynerlb
    |rewna
    |zsrc
    |jvcwa
    |ehgib
    |kivl
    |nkrz
    |hfjtb
    |tpws
    |ctvy
    |kunfrxuk
    |hktu
    |ldlx
    |dlgea
    |gugc
    |tzrba
    |tmwwa
    |ophl
    |tknz
    |syxoa
    |ctcr
    |efxxpemn
    |spxra
    |uakfa
    |vfjm
    |qkxe
    |lwvy
    |jdnoa
    |lbzwa
    |pgtda
    |nlxeb
    |gphe
    |njvl
    |nkfx
    |bqmp
    |chyqhvoj
    |ykjbdybd
    |ttlkydge
    |alku
    |uvds
    |zddwa
    |xiona
    |nbfza
    |ntio
    |vipba
    |fvwh
    |kuhi
    |kstlc
    |ezbt
    |tyix
    |jcnxa
    |urfgb
    |ionp
    |tfpy
    |iuhxb
    |niyka
    |qngb
    |dauza
    |zwjk
    |lsmy
    |kdnx
    |wuwdb
    |rcxsb
    |tkqhb
    |mgih
    |hxbla
    |wirp
    |ajpza
    |jkosa
    |kwzq
    |pfiba
    |wnfoa
    |ylsn
    |guvwb
    |lyifa
    |cgsk
    |omypa
    |hhfs
    |pzssa
    |jbmra
    |dmmd
    |fmsha
    |mmwz
    |unzc
    |jtqd
    |tniha
    |gllja
    |xwond
    |rhehfwvx
    |hrtw
    |jdzm
    |hlnu
    |swlqa
    |yxoya
    |ntdba
    |lcozezmw
    |qlfja
    |rwns
    |lrwx
    |lxjea
    |nmxg
    |otsq
    |cgjj
    |ddmg
    |wtvva
    |qodi
    |wpsqa
    |fjeha
    |paey
    |hjiq
    |frmkyvzf
    |nksl
    |ykilb
    |phyc
    |duny
    |cecra
    |jmudemvt
    |hjkva
    |lyvo
    |gzwm
    |ggno
    |tvypvekn
    |uzona
    |uoixa
    |chtqapyl
    |frmcb
    |qwabckqg
    |dzvwa
    |lzdha
    |aujva
    |ypwxqabp
    |opipb
    |urxob
    |cyrs
    |tqao
    |ashya
    |wpip
    |dkap
    |nhaga
    |qppl
    |umqxa
    |hvpidvag
    |vwfea
    |qzsxa
    |apipa
    |fsvgmppw
    |inergypr
    |fbde
    |bdqt
    |zalo
    |sjbzytzu
    |tijm
    |zjlw
    |gzxwa
    |qkirb
    |eotz
    |wpym
    |rtue
    |xvtdb
    |lfbryles
    |pqfga
    |ugksicsy
    |ffmv
    |byfvb
    |wfkba
    |rugy
    |iylca
    |hrye
    |wanm
    |qmsn
    |agmqa
    |otngpaee
    |tkvra
    |shlba
    |rvqf
    |gjjl
    |rpmn
    |fzug
    |bfbkb
    |ihjq
    |euve
    |aachd
    |njylmukt
    |fayb
    |onxgrwiq
    |zgiza
    |atrjdntp
    |gpncmjkn
    |omemucjl
    |dnieb
    |pywja
    |iuphmfcp
    |ztzzc
    |tqbeb
    |wifv
    |pvdrhkvd
    |yowe
    |temipobb
    |wrmrb
    |olfihkrg
    |gwwk
    |ydvu
    |ytqx
    |vsmh
    |oidf
    |huhya
    |vuwpa
    |avijhroz
    |actjipbg
    |ykqtutvu
    |espm
    |rtgrawwb
    |xchib
    |kydenpjs
    |dyzd
    |rbgpa
    |nlajb
    |mtqn
    |omhc
    |tluya
    |igwvb
    |csxx
    |barp
    |tmtw
    |caixb
    |qplga
    |bexja
    |vldsa
    |vejx
    |yywba
    |jtfw
    |yeds
    |uopi
    |jpjwa
    |zuzna
    |ugeb
    |mzfj
    |gplja
    |vben
    |anxd
    |ykziizgq
    |mbgnkprv
    |fbjf
    |nusw
    |agqp
    |dkmt
    |qqzu
    |yeckbgju
    |ylvhftoh
    |hfirmphb
    |jvbelskq
    |yrwea
    |tdzy
    |rtqv
    |xdepezjj
    |dstoa
    |styu
    |rktw
    |safmajhq
    |veudsqhd
    |guidhlpq
    |lfloyzec
    |agsrb
    |jgwg
    |anyyb
    |dibzlrhq
    |umai
    |xqeza
    |qrig
    |guvq
    |kmmg
    |bdioplym
    |brfe
    |pxlg
    |yowob
    |ixhs
    |btpw
    |nbqg
    |qkeva
    |aawe
    |wypt
    |blwd
    |ejeuxvrk
    |wcqs
    |piwwa
    |axktmpsi
    |nalnhtkc
    |zdyt
    |sbolrkno
    |egxq
    |kwxbvhex
    |qeny
    |zyxga
    |ytksb
    |kvts
    |hldrlzrl
    |vfzh
    |nsch
    |rugoa
    |etogb
    |knisa
    |iqova
    |jrsb
    |gdwxa
    |tphkjazl
    |cxhsa
    |giqxa
    |wceob
    |tpnh
    |kaoonuhy
    |jewoa
    |igjubjkx
    |ntrv
    |iikya
    |etjrtzat
    |uivm
    |fgqywrpq
    |ifro
    |ejcy
    |zlora
    |tqnl
    |ckxi
    |ttjua
    |wtdr
    |pbbe
    |rtqu
    |zlprofie
    |wbpca
    |lnlz
    |zrid
    |iihna
    |cpcfexkd
    |esim
    |aulzkhbh
    |azwniwpo
    |bzjvhpzj
    |jhrlseov
    |idgxqesv
    |xccya
    |mzaohqjg
    |abvx
    |ubknkevt
    |lixx
    |zmmw
    |mspxa
    |ykkp
    |jvfqppus
    |sbcga
    |vfivmelt
    |wgqsmnfq
    |zkkx
    |pxwfsygy
    |qdrg
    |humughwn
    |pgkt
    |rldnhnwk
    |skbwbogo
    |iqxzb
    |zxgmurxx
    |yuhg
    |swhn
    |rvsvlynu
    |bxvubznn
    |jsup
    |rxdq
    |nnnd
    |yshkiwik
    |dniujbnr
    |qbbcktkz
    |otduozro
    |xrcvdgak
    |uqxeacyh
    |kfxc
    |cgdevhmq
    |elluygep
    |wqwqskrm
    |shjwa
    |vtmi
    |bfwxa
    |ivruadns
    |oaldwcjq
    |zwrxtxet
    |puwvhqbe
    |uquiernf
    |jiqzfqdb
    |zbajctgx
    |gzwtmgzd
    |ysvkhpqq
    |ujhp
    |debj
    |dbdl
    |vqnw
    |fzgb
    |zhuvyuyl
    |pfwrb
    |qgvmqtmq
    |gnei
    |ljzo
    |tspf
    |hwfw
    |cwabxrvp
    |vuegdkzz
    |qafea
    |ppjwa
    |kwmua
    |libn
    |lkss
    |hzyn
    |ybbx
    |vfte
    |flppzwdk
    |qczy
    |skmpzrcn
    |vrsfgjpl
    |kmkyhejb
    |uvlzzrlw
    |qtkzpyhq
    |aggp
    |uicemdrl
    |iecjzyky
    |qotja
    |wnzna
    |nukjvfib
    |bmed
    |lhro
    |bifv
    |pyrm
    |ghgp
    |soac
    |hzuzwkec
    |sjdn
    |eeorzcap
    |uzlx
    |gwsfmlau
    |nmmisntc
    |bnju
    |neekezqj
    |vdkoeemh
    |hphe
    |wezx
    |xnegtvnu
    |hzwcbckv
    |paashzyg
    |cflz
    |kspjqujb
    |vexrxpjs
    |rrnm
    |napy
    |ldbl
    |esef
    |bmdv
    |meyf
    |vfhp
    |bflza
    |fpaxa
    |diig
    |gyhg
    |aygea
    |gzyu
    |yutja
    |jsqb
    |aweg
    |rldsa
    |vjxt
    |sbpjnery
    |brlmzmku
    |ltywpfsq
    |jwgqbjzs
    |oalga
    |qudzztwk
    |bmugylln
    |kvbc
    |qutm
    |ebyi
    |rxcxmfjf
    |hfdc
    |ngom
    |fqxv
    |xkov
    |fkuu
    |mjdma
    |zcku
    |ibmh
    |ugqw
    |ssaw
    |uryyghee
    |zuyf
    |crwe
    |atmmdilb
    |wujieuvf
    |jwnw
    |vwey
    |saoj
    |yvdu
    |mdrg
    |cskw
    |ewnk
    |vgsq
    |mvyo
    |ncfq
    |ypyh
    |vwti
    |akym
    |midz
    |rsxq
    |wzuv
    |cvbt
    |guwd
    |xncw
    |rjyxa
    |kuqk
    |efwg
    |ukuu
    |vavf
    |pirc
    |dzyf
    |hdxx
    |ucvr
    |ybfm
    |jlaf
    |lvos
    |tius
    |awgd
    |lfms
    |zvpj
    |iufd
    |tibm
    |gwsfmlau
    |blwd
    |eeorzcap
    |jwgqbjzs
    |jwnw
    |vbtncxif
  """.stripMargin.split("[\n,]").map(_.trim)

td.Main.setFastMode()

def analyzer(id: String) {
  if (!id.isEmpty && !id.startsWith("#"))
    TestRunner.runIdWithApron(id)
}

for (a <- toAnalyze) {
  analyzer(a)
}