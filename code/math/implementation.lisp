(in-package #:quaviver/math)

(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 32))
                          (unsigned-byte 32))
                round-to-odd/32)
         (ftype (function ((unsigned-byte 128) (unsigned-byte 64))
                          (unsigned-byte 64))
                round-to-odd/64)
         (ftype (function (fixnum) (unsigned-byte 64))
                integer-expt10/32)
         (ftype (function (fixnum) (unsigned-byte 128))
                integer-expt10/64)
         (ftype (function (fixnum) fixnum)
                floor-log2-expt10)
         (ftype (function (fixnum &optional boolean) fixnum)
                floor-log10-expt2)
         (inline round-to-odd/32
                 round-to-odd/64
                 integer-expt10/32
                 integer-expt10/64
                 floor-log2-expt10
                 floor-log10-expt2))

(defun round-to-odd/32 (g cp)
  (let ((p (* g cp)))
    (logior (ldb (byte 32 64) p)
            (if (> (ldb (byte 32 32) p) 1) 1 0))))

(defun round-to-odd/64 (g cp)
  (let ((p (* g cp)))
    (logior (ldb (byte 64 128) p)
            (if (> (ldb (byte 64 64) p) 1) 1 0))))

(defun compute-expt10 (k-min k-max width &optional (base 10l0))
  (make-array (- k-max k-min -1)
              :element-type `(integer 0 ,(1- (ash 1 width)))
              :initial-contents (loop for k from k-min to k-max
                                      for l = (expt base k)
                                      collect (ldb (byte width 0)
                                                   (ceiling (* (expt 2l0 (- width (floor (log l 2.0l0)) 1))
                                                               l))))))

(defvar *expt10/values/32*
  #(#x81CEB32C4B43FCF5   ; -31
    #xA2425FF75E14FC32   ; -30
    #xCAD2F7F5359A3B3F   ; -29
    #xFD87B5F28300CA0E   ; -28
    #x9E74D1B791E07E49   ; -27
    #xC612062576589DDB   ; -26
    #xF79687AED3EEC552   ; -25
    #x9ABE14CD44753B53   ; -24
    #xC16D9A0095928A28   ; -23
    #xF1C90080BAF72CB2   ; -22
    #x971DA05074DA7BEF   ; -21
    #xBCE5086492111AEB   ; -20
    #xEC1E4A7DB69561A6   ; -19
    #x9392EE8E921D5D08   ; -18
    #xB877AA3236A4B44A   ; -17
    #xE69594BEC44DE15C   ; -16
    #x901D7CF73AB0ACDA   ; -15
    #xB424DC35095CD810   ; -14
    #xE12E13424BB40E14   ; -13
    #x8CBCCC096F5088CC   ; -12
    #xAFEBFF0BCB24AAFF   ; -11
    #xDBE6FECEBDEDD5BF   ; -10
    #x89705F4136B4A598   ;  -9
    #xABCC77118461CEFD   ;  -8
    #xD6BF94D5E57A42BD   ;  -7
    #x8637BD05AF6C69B6   ;  -6
    #xA7C5AC471B478424   ;  -5
    #xD1B71758E219652C   ;  -4
    #x83126E978D4FDF3C   ;  -3
    #xA3D70A3D70A3D70B   ;  -2
    #xCCCCCCCCCCCCCCCD   ;  -1
    #x8000000000000000   ;   0
    #xA000000000000000   ;   1
    #xC800000000000000   ;   2
    #xFA00000000000000   ;   3
    #x9C40000000000000   ;   4
    #xC350000000000000   ;   5
    #xF424000000000000   ;   6
    #x9896800000000000   ;   7
    #xBEBC200000000000   ;   8
    #xEE6B280000000000   ;   9
    #x9502F90000000000   ;  10
    #xBA43B74000000000   ;  11
    #xE8D4A51000000000   ;  12
    #x9184E72A00000000   ;  13
    #xB5E620F480000000   ;  14
    #xE35FA931A0000000   ;  15
    #x8E1BC9BF04000000   ;  16
    #xB1A2BC2EC5000000   ;  17
    #xDE0B6B3A76400000   ;  18
    #x8AC7230489E80000   ;  19
    #xAD78EBC5AC620000   ;  20
    #xD8D726B7177A8000   ;  21
    #x878678326EAC9000   ;  22
    #xA968163F0A57B400   ;  23
    #xD3C21BCECCEDA100   ;  24
    #x84595161401484A0   ;  25
    #xA56FA5B99019A5C8   ;  26
    #xCECB8F27F4200F3A   ;  27
    #x813F3978F8940985   ;  28
    #xA18F07D736B90BE6   ;  29
    #xC9F2C9CD04674EDF   ;  30
    #xFC6F7C4045812297   ;  31
    #x9DC5ADA82B70B59E   ;  32
    #xC5371912364CE306   ;  33
    #xF684DF56C3E01BC7   ;  34
    #x9A130B963A6C115D   ;  35
    #xC097CE7BC90715B4   ;  36
    #xF0BDC21ABB48DB21   ;  37
    #x96769950B50D88F5   ;  38
    #xBC143FA4E250EB32   ;  39
    #xEB194F8E1AE525FE   ;  40
    #x92EFD1B8D0CF37BF   ;  41
    #xB7ABC627050305AE   ;  42
    #xE596B7B0C643C71A   ;  43
    #x8F7E32CE7BEA5C70   ;  44
    #xB35DBF821AE4F38C   ;  45
    #xE0352F62A19E306F   ;  46
    #x8C213D9DA502DE46   ;  47
    #xAF298D050E4395D7   ;  48
    #xDAF3F04651D47B4D   ;  49
    #x88D8762BF324CD10   ;  50
    #xAB0E93B6EFEE0054   ;  51
    #xD5D238A4ABE98069)) ;  52

(defconstant +expt10/min-exponent/32 -31)

(defconstant +expt10/max-exponent/32 52)

(defun integer-expt10/32 (power)
  (svref *expt10/values/32*
         (- (- +expt10/min-exponent/32) power)))

(defvar *expt10/values/64*
  #(#xFF77B1FCBEBCDC4F25E8E89C13BB0F7B   ; -292
    #x9FAACF3DF73609B177B191618C54E9AD   ; -291
    #xC795830D75038C1DD59DF5B9EF6A2418   ; -290
    #xF97AE3D0D2446F254B0573286B44AD1E   ; -289
    #x9BECCE62836AC5774EE367F9430AEC33   ; -288
    #xC2E801FB244576D5229C41F793CDA740   ; -287
    #xF3A20279ED56D48A6B43527578C11110   ; -286
    #x9845418C345644D6830A13896B78AAAA   ; -285
    #xBE5691EF416BD60C23CC986BC656D554   ; -284
    #xEDEC366B11C6CB8F2CBFBE86B7EC8AA9   ; -283
    #x94B3A202EB1C3F397BF7D71432F3D6AA   ; -282
    #xB9E08A83A5E34F07DAF5CCD93FB0CC54   ; -281
    #xE858AD248F5C22C9D1B3400F8F9CFF69   ; -280
    #x91376C36D99995BE23100809B9C21FA2   ; -279
    #xB58547448FFFFB2DABD40A0C2832A78B   ; -278
    #xE2E69915B3FFF9F916C90C8F323F516D   ; -277
    #x8DD01FAD907FFC3BAE3DA7D97F6792E4   ; -276
    #xB1442798F49FFB4A99CD11CFDF41779D   ; -275
    #xDD95317F31C7FA1D40405643D711D584   ; -274
    #x8A7D3EEF7F1CFC52482835EA666B2573   ; -273
    #xAD1C8EAB5EE43B66DA3243650005EED0   ; -272
    #xD863B256369D4A4090BED43E40076A83   ; -271
    #x873E4F75E2224E685A7744A6E804A292   ; -270
    #xA90DE3535AAAE202711515D0A205CB37   ; -269
    #xD3515C2831559A830D5A5B44CA873E04   ; -268
    #x8412D9991ED58091E858790AFE9486C3   ; -267
    #xA5178FFF668AE0B6626E974DBE39A873   ; -266
    #xCE5D73FF402D98E3FB0A3D212DC81290   ; -265
    #x80FA687F881C7F8E7CE66634BC9D0B9A   ; -264
    #xA139029F6A239F721C1FFFC1EBC44E81   ; -263
    #xC987434744AC874EA327FFB266B56221   ; -262
    #xFBE9141915D7A9224BF1FF9F0062BAA9   ; -261
    #x9D71AC8FADA6C9B56F773FC3603DB4AA   ; -260
    #xC4CE17B399107C22CB550FB4384D21D4   ; -259
    #xF6019DA07F549B2B7E2A53A146606A49   ; -258
    #x99C102844F94E0FB2EDA7444CBFC426E   ; -257
    #xC0314325637A1939FA911155FEFB5309   ; -256
    #xF03D93EEBC589F88793555AB7EBA27CB   ; -255
    #x96267C7535B763B54BC1558B2F3458DF   ; -254
    #xBBB01B9283253CA29EB1AAEDFB016F17   ; -253
    #xEA9C227723EE8BCB465E15A979C1CADD   ; -252
    #x92A1958A7675175F0BFACD89EC191ECA   ; -251
    #xB749FAED14125D36CEF980EC671F667C   ; -250
    #xE51C79A85916F48482B7E12780E7401B   ; -249
    #x8F31CC0937AE58D2D1B2ECB8B0908811   ; -248
    #xB2FE3F0B8599EF07861FA7E6DCB4AA16   ; -247
    #xDFBDCECE67006AC967A791E093E1D49B   ; -246
    #x8BD6A141006042BDE0C8BB2C5C6D24E1   ; -245
    #xAECC49914078536D58FAE9F773886E19   ; -244
    #xDA7F5BF590966848AF39A475506A899F   ; -243
    #x888F99797A5E012D6D8406C952429604   ; -242
    #xAAB37FD7D8F58178C8E5087BA6D33B84   ; -241
    #xD5605FCDCF32E1D6FB1E4A9A90880A65   ; -240
    #x855C3BE0A17FCD265CF2EEA09A550680   ; -239
    #xA6B34AD8C9DFC06FF42FAA48C0EA481F   ; -238
    #xD0601D8EFC57B08BF13B94DAF124DA27   ; -237
    #x823C12795DB6CE5776C53D08D6B70859   ; -236
    #xA2CB1717B52481ED54768C4B0C64CA6F   ; -235
    #xCB7DDCDDA26DA268A9942F5DCF7DFD0A   ; -234
    #xFE5D54150B090B02D3F93B35435D7C4D   ; -233
    #x9EFA548D26E5A6E1C47BC5014A1A6DB0   ; -232
    #xC6B8E9B0709F109A359AB6419CA1091C   ; -231
    #xF867241C8CC6D4C0C30163D203C94B63   ; -230
    #x9B407691D7FC44F879E0DE63425DCF1E   ; -229
    #xC21094364DFB5636985915FC12F542E5   ; -228
    #xF294B943E17A2BC43E6F5B7B17B2939E   ; -227
    #x979CF3CA6CEC5B5AA705992CEECF9C43   ; -226
    #xBD8430BD0827723150C6FF782A838354   ; -225
    #xECE53CEC4A314EBDA4F8BF5635246429   ; -224
    #x940F4613AE5ED136871B7795E136BE9A   ; -223
    #xB913179899F6858428E2557B59846E40   ; -222
    #xE757DD7EC07426E5331AEADA2FE589D0   ; -221
    #x9096EA6F3848984F3FF0D2C85DEF7622   ; -220
    #xB4BCA50B065ABE630FED077A756B53AA   ; -219
    #xE1EBCE4DC7F16DFBD3E8495912C62895   ; -218
    #x8D3360F09CF6E4BD64712DD7ABBBD95D   ; -217
    #xB080392CC4349DECBD8D794D96AACFB4   ; -216
    #xDCA04777F541C567ECF0D7A0FC5583A1   ; -215
    #x89E42CAAF9491B60F41686C49DB57245   ; -214
    #xAC5D37D5B79B6239311C2875C522CED6   ; -213
    #xD77485CB25823AC77D633293366B828C   ; -212
    #x86A8D39EF77164BCAE5DFF9C02033198   ; -211
    #xA8530886B54DBDEBD9F57F830283FDFD   ; -210
    #xD267CAA862A12D66D072DF63C324FD7C   ; -209
    #x8380DEA93DA4BC604247CB9E59F71E6E   ; -208
    #xA46116538D0DEB7852D9BE85F074E609   ; -207
    #xCD795BE87051665667902E276C921F8C   ; -206
    #x806BD9714632DFF600BA1CD8A3DB53B7   ; -205
    #xA086CFCD97BF97F380E8A40ECCD228A5   ; -204
    #xC8A883C0FDAF7DF06122CD128006B2CE   ; -203
    #xFAD2A4B13D1B5D6C796B805720085F82   ; -202
    #x9CC3A6EEC6311A63CBE3303674053BB1   ; -201
    #xC3F490AA77BD60FCBEDBFC4411068A9D   ; -200
    #xF4F1B4D515ACB93BEE92FB5515482D45   ; -199
    #x991711052D8BF3C5751BDD152D4D1C4B   ; -198
    #xBF5CD54678EEF0B6D262D45A78A0635E   ; -197
    #xEF340A98172AACE486FB897116C87C35   ; -196
    #x9580869F0E7AAC0ED45D35E6AE3D4DA1   ; -195
    #xBAE0A846D21957128974836059CCA10A   ; -194
    #xE998D258869FACD72BD1A438703FC94C   ; -193
    #x91FF83775423CC067B6306A34627DDD0   ; -192
    #xB67F6455292CBF081A3BC84C17B1D543   ; -191
    #xE41F3D6A7377EECA20CABA5F1D9E4A94   ; -190
    #x8E938662882AF53E547EB47B7282EE9D   ; -189
    #xB23867FB2A35B28DE99E619A4F23AA44   ; -188
    #xDEC681F9F4C31F316405FA00E2EC94D5   ; -187
    #x8B3C113C38F9F37EDE83BC408DD3DD05   ; -186
    #xAE0B158B4738705E9624AB50B148D446   ; -185
    #xD98DDAEE19068C763BADD624DD9B0958   ; -184
    #x87F8A8D4CFA417C9E54CA5D70A80E5D7   ; -183
    #xA9F6D30A038D1DBC5E9FCF4CCD211F4D   ; -182
    #xD47487CC8470652B7647C32000696720   ; -181
    #x84C8D4DFD2C63F3B29ECD9F40041E074   ; -180
    #xA5FB0A17C777CF09F468107100525891   ; -179
    #xCF79CC9DB955C2CC7182148D4066EEB5   ; -178
    #x81AC1FE293D599BFC6F14CD848405531   ; -177
    #xA21727DB38CB002FB8ADA00E5A506A7D   ; -176
    #xCA9CF1D206FDC03BA6D90811F0E4851D   ; -175
    #xFD442E4688BD304A908F4A166D1DA664   ; -174
    #x9E4A9CEC15763E2E9A598E4E043287FF   ; -173
    #xC5DD44271AD3CDBA40EFF1E1853F29FE   ; -172
    #xF7549530E188C128D12BEE59E68EF47D   ; -171
    #x9A94DD3E8CF578B982BB74F8301958CF   ; -170
    #xC13A148E3032D6E7E36A52363C1FAF02   ; -169
    #xF18899B1BC3F8CA1DC44E6C3CB279AC2   ; -168
    #x96F5600F15A7B7E529AB103A5EF8C0BA   ; -167
    #xBCB2B812DB11A5DE7415D448F6B6F0E8   ; -166
    #xEBDF661791D60F56111B495B3464AD22   ; -165
    #x936B9FCEBB25C995CAB10DD900BEEC35   ; -164
    #xB84687C269EF3BFB3D5D514F40EEA743   ; -163
    #xE65829B3046B0AFA0CB4A5A3112A5113   ; -162
    #x8FF71A0FE2C2E6DC47F0E785EABA72AC   ; -161
    #xB3F4E093DB73A09359ED216765690F57   ; -160
    #xE0F218B8D25088B8306869C13EC3532D   ; -159
    #x8C974F73837255731E414218C73A13FC   ; -158
    #xAFBD2350644EEACFE5D1929EF90898FB   ; -157
    #xDBAC6C247D62A583DF45F746B74ABF3A   ; -156
    #x894BC396CE5DA7726B8BBA8C328EB784   ; -155
    #xAB9EB47C81F5114F066EA92F3F326565   ; -154
    #xD686619BA27255A2C80A537B0EFEFEBE   ; -153
    #x8613FD0145877585BD06742CE95F5F37   ; -152
    #xA798FC4196E952E72C48113823B73705   ; -151
    #xD17F3B51FCA3A7A0F75A15862CA504C6   ; -150
    #x82EF85133DE648C49A984D73DBE722FC   ; -149
    #xA3AB66580D5FDAF5C13E60D0D2E0EBBB   ; -148
    #xCC963FEE10B7D1B3318DF905079926A9   ; -147
    #xFFBBCFE994E5C61FFDF17746497F7053   ; -146
    #x9FD561F1FD0F9BD3FEB6EA8BEDEFA634   ; -145
    #xC7CABA6E7C5382C8FE64A52EE96B8FC1   ; -144
    #xF9BD690A1B68637B3DFDCE7AA3C673B1   ; -143
    #x9C1661A651213E2D06BEA10CA65C084F   ; -142
    #xC31BFA0FE5698DB8486E494FCFF30A63   ; -141
    #xF3E2F893DEC3F1265A89DBA3C3EFCCFB   ; -140
    #x986DDB5C6B3A76B7F89629465A75E01D   ; -139
    #xBE89523386091465F6BBB397F1135824   ; -138
    #xEE2BA6C0678B597F746AA07DED582E2D   ; -137
    #x94DB483840B717EFA8C2A44EB4571CDD   ; -136
    #xBA121A4650E4DDEB92F34D62616CE414   ; -135
    #xE896A0D7E51E156677B020BAF9C81D18   ; -134
    #x915E2486EF32CD600ACE1474DC1D122F   ; -133
    #xB5B5ADA8AAFF80B80D819992132456BB   ; -132
    #xE3231912D5BF60E610E1FFF697ED6C6A   ; -131
    #x8DF5EFABC5979C8FCA8D3FFA1EF463C2   ; -130
    #xB1736B96B6FD83B3BD308FF8A6B17CB3   ; -129
    #xDDD0467C64BCE4A0AC7CB3F6D05DDBDF   ; -128
    #x8AA22C0DBEF60EE46BCDF07A423AA96C   ; -127
    #xAD4AB7112EB3929D86C16C98D2C953C7   ; -126
    #xD89D64D57A607744E871C7BF077BA8B8   ; -125
    #x87625F056C7C4A8B11471CD764AD4973   ; -124
    #xA93AF6C6C79B5D2DD598E40D3DD89BD0   ; -123
    #xD389B478798234794AFF1D108D4EC2C4   ; -122
    #x843610CB4BF160CBCEDF722A585139BB   ; -121
    #xA54394FE1EEDB8FEC2974EB4EE658829   ; -120
    #xCE947A3DA6A9273E733D226229FEEA33   ; -119
    #x811CCC668829B8870806357D5A3F5260   ; -118
    #xA163FF802A3426A8CA07C2DCB0CF26F8   ; -117
    #xC9BCFF6034C13052FC89B393DD02F0B6   ; -116
    #xFC2C3F3841F17C67BBAC2078D443ACE3   ; -115
    #x9D9BA7832936EDC0D54B944B84AA4C0E   ; -114
    #xC5029163F384A9310A9E795E65D4DF12   ; -113
    #xF64335BCF065D37D4D4617B5FF4A16D6   ; -112
    #x99EA0196163FA42E504BCED1BF8E4E46   ; -111
    #xC06481FB9BCF8D39E45EC2862F71E1D7   ; -110
    #xF07DA27A82C370885D767327BB4E5A4D   ; -109
    #x964E858C91BA26553A6A07F8D510F870   ; -108
    #xBBE226EFB628AFEA890489F70A55368C   ; -107
    #xEADAB0ABA3B2DBE52B45AC74CCEA842F   ; -106
    #x92C8AE6B464FC96F3B0B8BC90012929E   ; -105
    #xB77ADA0617E3BBCB09CE6EBB40173745   ; -104
    #xE55990879DDCAABDCC420A6A101D0516   ; -103
    #x8F57FA54C2A9EAB69FA946824A12232E   ; -102
    #xB32DF8E9F354656447939822DC96ABFA   ; -101
    #xDFF9772470297EBD59787E2B93BC56F8   ; -100
    #x8BFBEA76C619EF3657EB4EDB3C55B65B   ;  -99
    #xAEFAE51477A06B03EDE622920B6B23F2   ;  -98
    #xDAB99E59958885C4E95FAB368E45ECEE   ;  -97
    #x88B402F7FD75539B11DBCB0218EBB415   ;  -96
    #xAAE103B5FCD2A881D652BDC29F26A11A   ;  -95
    #xD59944A37C0752A24BE76D3346F04960   ;  -94
    #x857FCAE62D8493A56F70A4400C562DDC   ;  -93
    #xA6DFBD9FB8E5B88ECB4CCD500F6BB953   ;  -92
    #xD097AD07A71F26B27E2000A41346A7A8   ;  -91
    #x825ECC24C873782F8ED400668C0C28C9   ;  -90
    #xA2F67F2DFA90563B728900802F0F32FB   ;  -89
    #xCBB41EF979346BCA4F2B40A03AD2FFBA   ;  -88
    #xFEA126B7D78186BCE2F610C84987BFA9   ;  -87
    #x9F24B832E6B0F4360DD9CA7D2DF4D7CA   ;  -86
    #xC6EDE63FA05D314391503D1C79720DBC   ;  -85
    #xF8A95FCF88747D9475A44C6397CE912B   ;  -84
    #x9B69DBE1B548CE7CC986AFBE3EE11ABB   ;  -83
    #xC24452DA229B021BFBE85BADCE996169   ;  -82
    #xF2D56790AB41C2A2FAE27299423FB9C4   ;  -81
    #x97C560BA6B0919A5DCCD879FC967D41B   ;  -80
    #xBDB6B8E905CB600F5400E987BBC1C921   ;  -79
    #xED246723473E3813290123E9AAB23B69   ;  -78
    #x9436C0760C86E30BF9A0B6720AAF6522   ;  -77
    #xB94470938FA89BCEF808E40E8D5B3E6A   ;  -76
    #xE7958CB87392C2C2B60B1D1230B20E05   ;  -75
    #x90BD77F3483BB9B9B1C6F22B5E6F48C3   ;  -74
    #xB4ECD5F01A4AA8281E38AEB6360B1AF4   ;  -73
    #xE2280B6C20DD523225C6DA63C38DE1B1   ;  -72
    #x8D590723948A535F579C487E5A38AD0F   ;  -71
    #xB0AF48EC79ACE8372D835A9DF0C6D852   ;  -70
    #xDCDB1B2798182244F8E431456CF88E66   ;  -69
    #x8A08F0F8BF0F156B1B8E9ECB641B5900   ;  -68
    #xAC8B2D36EED2DAC5E272467E3D222F40   ;  -67
    #xD7ADF884AA8791775B0ED81DCC6ABB10   ;  -66
    #x86CCBB52EA94BAEA98E947129FC2B4EA   ;  -65
    #xA87FEA27A539E9A53F2398D747B36225   ;  -64
    #xD29FE4B18E88640E8EEC7F0D19A03AAE   ;  -63
    #x83A3EEEEF9153E891953CF68300424AD   ;  -62
    #xA48CEAAAB75A8E2B5FA8C3423C052DD8   ;  -61
    #xCDB02555653131B63792F412CB06794E   ;  -60
    #x808E17555F3EBF11E2BBD88BBEE40BD1   ;  -59
    #xA0B19D2AB70E6ED65B6ACEAEAE9D0EC5   ;  -58
    #xC8DE047564D20A8BF245825A5A445276   ;  -57
    #xFB158592BE068D2EEED6E2F0F0D56713   ;  -56
    #x9CED737BB6C4183D55464DD69685606C   ;  -55
    #xC428D05AA4751E4CAA97E14C3C26B887   ;  -54
    #xF53304714D9265DFD53DD99F4B3066A9   ;  -53
    #x993FE2C6D07B7FABE546A8038EFE402A   ;  -52
    #xBF8FDB78849A5F96DE98520472BDD034   ;  -51
    #xEF73D256A5C0F77C963E66858F6D4441   ;  -50
    #x95A8637627989AADDDE7001379A44AA9   ;  -49
    #xBB127C53B17EC1595560C018580D5D53   ;  -48
    #xE9D71B689DDE71AFAAB8F01E6E10B4A7   ;  -47
    #x9226712162AB070DCAB3961304CA70E9   ;  -46
    #xB6B00D69BB55C8D13D607B97C5FD0D23   ;  -45
    #xE45C10C42A2B3B058CB89A7DB77C506B   ;  -44
    #x8EB98A7A9A5B04E377F3608E92ADB243   ;  -43
    #xB267ED1940F1C61C55F038B237591ED4   ;  -42
    #xDF01E85F912E37A36B6C46DEC52F6689   ;  -41
    #x8B61313BBABCE2C62323AC4B3B3DA016   ;  -40
    #xAE397D8AA96C1B77ABEC975E0A0D081B   ;  -39
    #xD9C7DCED53C7225596E7BD358C904A22   ;  -38
    #x881CEA14545C75757E50D64177DA2E55   ;  -37
    #xAA242499697392D2DDE50BD1D5D0B9EA   ;  -36
    #xD4AD2DBFC3D07787955E4EC64B44E865   ;  -35
    #x84EC3C97DA624AB4BD5AF13BEF0B113F   ;  -34
    #xA6274BBDD0FADD61ECB1AD8AEACDD58F   ;  -33
    #xCFB11EAD453994BA67DE18EDA5814AF3   ;  -32
    #x81CEB32C4B43FCF480EACF948770CED8   ;  -31
    #xA2425FF75E14FC31A1258379A94D028E   ;  -30
    #xCAD2F7F5359A3B3E096EE45813A04331   ;  -29
    #xFD87B5F28300CA0D8BCA9D6E188853FD   ;  -28
    #x9E74D1B791E07E48775EA264CF55347E   ;  -27
    #xC612062576589DDA95364AFE032A819E   ;  -26
    #xF79687AED3EEC5513A83DDBD83F52205   ;  -25
    #x9ABE14CD44753B52C4926A9672793543   ;  -24
    #xC16D9A0095928A2775B7053C0F178294   ;  -23
    #xF1C90080BAF72CB15324C68B12DD6339   ;  -22
    #x971DA05074DA7BEED3F6FC16EBCA5E04   ;  -21
    #xBCE5086492111AEA88F4BB1CA6BCF585   ;  -20
    #xEC1E4A7DB69561A52B31E9E3D06C32E6   ;  -19
    #x9392EE8E921D5D073AFF322E62439FD0   ;  -18
    #xB877AA3236A4B44909BEFEB9FAD487C3   ;  -17
    #xE69594BEC44DE15B4C2EBE687989A9B4   ;  -16
    #x901D7CF73AB0ACD90F9D37014BF60A11   ;  -15
    #xB424DC35095CD80F538484C19EF38C95   ;  -14
    #xE12E13424BB40E132865A5F206B06FBA   ;  -13
    #x8CBCCC096F5088CBF93F87B7442E45D4   ;  -12
    #xAFEBFF0BCB24AAFEF78F69A51539D749   ;  -11
    #xDBE6FECEBDEDD5BEB573440E5A884D1C   ;  -10
    #x89705F4136B4A59731680A88F8953031   ;   -9
    #xABCC77118461CEFCFDC20D2B36BA7C3E   ;   -8
    #xD6BF94D5E57A42BC3D32907604691B4D   ;   -7
    #x8637BD05AF6C69B5A63F9A49C2C1B110   ;   -6
    #xA7C5AC471B4784230FCF80DC33721D54   ;   -5
    #xD1B71758E219652BD3C36113404EA4A9   ;   -4
    #x83126E978D4FDF3B645A1CAC083126EA   ;   -3
    #xA3D70A3D70A3D70A3D70A3D70A3D70A4   ;   -2
    #xCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCD   ;   -1
    #x80000000000000000000000000000000   ;    0
    #xA0000000000000000000000000000000   ;    1
    #xC8000000000000000000000000000000   ;    2
    #xFA000000000000000000000000000000   ;    3
    #x9C400000000000000000000000000000   ;    4
    #xC3500000000000000000000000000000   ;    5
    #xF4240000000000000000000000000000   ;    6
    #x98968000000000000000000000000000   ;    7
    #xBEBC2000000000000000000000000000   ;    8
    #xEE6B2800000000000000000000000000   ;    9
    #x9502F900000000000000000000000000   ;   10
    #xBA43B740000000000000000000000000   ;   11
    #xE8D4A510000000000000000000000000   ;   12
    #x9184E72A000000000000000000000000   ;   13
    #xB5E620F4800000000000000000000000   ;   14
    #xE35FA931A00000000000000000000000   ;   15
    #x8E1BC9BF040000000000000000000000   ;   16
    #xB1A2BC2EC50000000000000000000000   ;   17
    #xDE0B6B3A764000000000000000000000   ;   18
    #x8AC7230489E800000000000000000000   ;   19
    #xAD78EBC5AC6200000000000000000000   ;   20
    #xD8D726B7177A80000000000000000000   ;   21
    #x878678326EAC90000000000000000000   ;   22
    #xA968163F0A57B4000000000000000000   ;   23
    #xD3C21BCECCEDA1000000000000000000   ;   24
    #x84595161401484A00000000000000000   ;   25
    #xA56FA5B99019A5C80000000000000000   ;   26
    #xCECB8F27F4200F3A0000000000000000   ;   27
    #x813F3978F89409844000000000000000   ;   28
    #xA18F07D736B90BE55000000000000000   ;   29
    #xC9F2C9CD04674EDEA400000000000000   ;   30
    #xFC6F7C40458122964D00000000000000   ;   31
    #x9DC5ADA82B70B59DF020000000000000   ;   32
    #xC5371912364CE3056C28000000000000   ;   33
    #xF684DF56C3E01BC6C732000000000000   ;   34
    #x9A130B963A6C115C3C7F400000000000   ;   35
    #xC097CE7BC90715B34B9F100000000000   ;   36
    #xF0BDC21ABB48DB201E86D40000000000   ;   37
    #x96769950B50D88F41314448000000000   ;   38
    #xBC143FA4E250EB3117D955A000000000   ;   39
    #xEB194F8E1AE525FD5DCFAB0800000000   ;   40
    #x92EFD1B8D0CF37BE5AA1CAE500000000   ;   41
    #xB7ABC627050305ADF14A3D9E40000000   ;   42
    #xE596B7B0C643C7196D9CCD05D0000000   ;   43
    #x8F7E32CE7BEA5C6FE4820023A2000000   ;   44
    #xB35DBF821AE4F38BDDA2802C8A800000   ;   45
    #xE0352F62A19E306ED50B2037AD200000   ;   46
    #x8C213D9DA502DE454526F422CC340000   ;   47
    #xAF298D050E4395D69670B12B7F410000   ;   48
    #xDAF3F04651D47B4C3C0CDD765F114000   ;   49
    #x88D8762BF324CD0FA5880A69FB6AC800   ;   50
    #xAB0E93B6EFEE00538EEA0D047A457A00   ;   51
    #xD5D238A4ABE9806872A4904598D6D880   ;   52
    #x85A36366EB71F04147A6DA2B7F864750   ;   53
    #xA70C3C40A64E6C51999090B65F67D924   ;   54
    #xD0CF4B50CFE20765FFF4B4E3F741CF6D   ;   55
    #x82818F1281ED449FBFF8F10E7A8921A5   ;   56
    #xA321F2D7226895C7AFF72D52192B6A0E   ;   57
    #xCBEA6F8CEB02BB399BF4F8A69F764491   ;   58
    #xFEE50B7025C36A0802F236D04753D5B5   ;   59
    #x9F4F2726179A224501D762422C946591   ;   60
    #xC722F0EF9D80AAD6424D3AD2B7B97EF6   ;   61
    #xF8EBAD2B84E0D58BD2E0898765A7DEB3   ;   62
    #x9B934C3B330C857763CC55F49F88EB30   ;   63
    #xC2781F49FFCFA6D53CBF6B71C76B25FC   ;   64
    #xF316271C7FC3908A8BEF464E3945EF7B   ;   65
    #x97EDD871CFDA3A5697758BF0E3CBB5AD   ;   66
    #xBDE94E8E43D0C8EC3D52EEED1CBEA318   ;   67
    #xED63A231D4C4FB274CA7AAA863EE4BDE   ;   68
    #x945E455F24FB1CF88FE8CAA93E74EF6B   ;   69
    #xB975D6B6EE39E436B3E2FD538E122B45   ;   70
    #xE7D34C64A9C85D4460DBBCA87196B617   ;   71
    #x90E40FBEEA1D3A4ABC8955E946FE31CE   ;   72
    #xB51D13AEA4A488DD6BABAB6398BDBE42   ;   73
    #xE264589A4DCDAB14C696963C7EED2DD2   ;   74
    #x8D7EB76070A08AECFC1E1DE5CF543CA3   ;   75
    #xB0DE65388CC8ADA83B25A55F43294BCC   ;   76
    #xDD15FE86AFFAD91249EF0EB713F39EBF   ;   77
    #x8A2DBF142DFCC7AB6E3569326C784338   ;   78
    #xACB92ED9397BF99649C2C37F07965405   ;   79
    #xD7E77A8F87DAF7FBDC33745EC97BE907   ;   80
    #x86F0AC99B4E8DAFD69A028BB3DED71A4   ;   81
    #xA8ACD7C0222311BCC40832EA0D68CE0D   ;   82
    #xD2D80DB02AABD62BF50A3FA490C30191   ;   83
    #x83C7088E1AAB65DB792667C6DA79E0FB   ;   84
    #xA4B8CAB1A1563F52577001B891185939   ;   85
    #xCDE6FD5E09ABCF26ED4C0226B55E6F87   ;   86
    #x80B05E5AC60B6178544F8158315B05B5   ;   87
    #xA0DC75F1778E39D6696361AE3DB1C722   ;   88
    #xC913936DD571C84C03BC3A19CD1E38EA   ;   89
    #xFB5878494ACE3A5F04AB48A04065C724   ;   90
    #x9D174B2DCEC0E47B62EB0D64283F9C77   ;   91
    #xC45D1DF942711D9A3BA5D0BD324F8395   ;   92
    #xF5746577930D6500CA8F44EC7EE3647A   ;   93
    #x9968BF6ABBE85F207E998B13CF4E1ECC   ;   94
    #xBFC2EF456AE276E89E3FEDD8C321A67F   ;   95
    #xEFB3AB16C59B14A2C5CFE94EF3EA101F   ;   96
    #x95D04AEE3B80ECE5BBA1F1D158724A13   ;   97
    #xBB445DA9CA61281F2A8A6E45AE8EDC98   ;   98
    #xEA1575143CF97226F52D09D71A3293BE   ;   99
    #x924D692CA61BE758593C2626705F9C57   ;  100
    #xB6E0C377CFA2E12E6F8B2FB00C77836D   ;  101
    #xE498F455C38B997A0B6DFB9C0F956448   ;  102
    #x8EDF98B59A373FEC4724BD4189BD5EAD   ;  103
    #xB2977EE300C50FE758EDEC91EC2CB658   ;  104
    #xDF3D5E9BC0F653E12F2967B66737E3EE   ;  105
    #x8B865B215899F46CBD79E0D20082EE75   ;  106
    #xAE67F1E9AEC07187ECD8590680A3AA12   ;  107
    #xDA01EE641A708DE9E80E6F4820CC9496   ;  108
    #x884134FE908658B23109058D147FDCDE   ;  109
    #xAA51823E34A7EEDEBD4B46F0599FD416   ;  110
    #xD4E5E2CDC1D1EA966C9E18AC7007C91B   ;  111
    #x850FADC09923329E03E2CF6BC604DDB1   ;  112
    #xA6539930BF6BFF4584DB8346B786151D   ;  113
    #xCFE87F7CEF46FF16E612641865679A64   ;  114
    #x81F14FAE158C5F6E4FCB7E8F3F60C07F   ;  115
    #xA26DA3999AEF7749E3BE5E330F38F09E   ;  116
    #xCB090C8001AB551C5CADF5BFD3072CC6   ;  117
    #xFDCB4FA002162A6373D9732FC7C8F7F7   ;  118
    #x9E9F11C4014DDA7E2867E7FDDCDD9AFB   ;  119
    #xC646D63501A1511DB281E1FD541501B9   ;  120
    #xF7D88BC24209A5651F225A7CA91A4227   ;  121
    #x9AE757596946075F3375788DE9B06959   ;  122
    #xC1A12D2FC39789370052D6B1641C83AF   ;  123
    #xF209787BB47D6B84C0678C5DBD23A49B   ;  124
    #x9745EB4D50CE6332F840B7BA963646E1   ;  125
    #xBD176620A501FBFFB650E5A93BC3D899   ;  126
    #xEC5D3FA8CE427AFFA3E51F138AB4CEBF   ;  127
    #x93BA47C980E98CDFC66F336C36B10138   ;  128
    #xB8A8D9BBE123F017B80B0047445D4185   ;  129
    #xE6D3102AD96CEC1DA60DC059157491E6   ;  130
    #x9043EA1AC7E4139287C89837AD68DB30   ;  131
    #xB454E4A179DD187729BABE4598C311FC   ;  132
    #xE16A1DC9D8545E94F4296DD6FEF3D67B   ;  133
    #x8CE2529E2734BB1D1899E4A65F58660D   ;  134
    #xB01AE745B101E9E45EC05DCFF72E7F90   ;  135
    #xDC21A1171D42645D76707543F4FA1F74   ;  136
    #x899504AE72497EBA6A06494A791C53A9   ;  137
    #xABFA45DA0EDBDE690487DB9D17636893   ;  138
    #xD6F8D7509292D60345A9D2845D3C42B7   ;  139
    #x865B86925B9BC5C20B8A2392BA45A9B3   ;  140
    #xA7F26836F282B7328E6CAC7768D7141F   ;  141
    #xD1EF0244AF2364FF3207D795430CD927   ;  142
    #x8335616AED761F1F7F44E6BD49E807B9   ;  143
    #xA402B9C5A8D3A6E75F16206C9C6209A7   ;  144
    #xCD036837130890A136DBA887C37A8C10   ;  145
    #x802221226BE55A64C2494954DA2C978A   ;  146
    #xA02AA96B06DEB0FDF2DB9BAA10B7BD6D   ;  147
    #xC83553C5C8965D3D6F92829494E5ACC8   ;  148
    #xFA42A8B73ABBF48CCB772339BA1F17FA   ;  149
    #x9C69A97284B578D7FF2A760414536EFC   ;  150
    #xC38413CF25E2D70DFEF5138519684ABB   ;  151
    #xF46518C2EF5B8CD17EB258665FC25D6A   ;  152
    #x98BF2F79D5993802EF2F773FFBD97A62   ;  153
    #xBEEEFB584AFF8603AAFB550FFACFD8FB   ;  154
    #xEEAABA2E5DBF678495BA2A53F983CF39   ;  155
    #x952AB45CFA97A0B2DD945A747BF26184   ;  156
    #xBA756174393D88DF94F971119AEEF9E5   ;  157
    #xE912B9D1478CEB177A37CD5601AAB85E   ;  158
    #x91ABB422CCB812EEAC62E055C10AB33B   ;  159
    #xB616A12B7FE617AA577B986B314D600A   ;  160
    #xE39C49765FDF9D94ED5A7E85FDA0B80C   ;  161
    #x8E41ADE9FBEBC27D14588F13BE847308   ;  162
    #xB1D219647AE6B31C596EB2D8AE258FC9   ;  163
    #xDE469FBD99A05FE36FCA5F8ED9AEF3BC   ;  164
    #x8AEC23D680043BEE25DE7BB9480D5855   ;  165
    #xADA72CCC20054AE9AF561AA79A10AE6B   ;  166
    #xD910F7FF28069DA41B2BA1518094DA05   ;  167
    #x87AA9AFF7904228690FB44D2F05D0843   ;  168
    #xA99541BF57452B28353A1607AC744A54   ;  169
    #xD3FA922F2D1675F242889B8997915CE9   ;  170
    #x847C9B5D7C2E09B769956135FEBADA12   ;  171
    #xA59BC234DB398C2543FAB9837E699096   ;  172
    #xCF02B2C21207EF2E94F967E45E03F4BC   ;  173
    #x8161AFB94B44F57D1D1BE0EEBAC278F6   ;  174
    #xA1BA1BA79E1632DC6462D92A69731733   ;  175
    #xCA28A291859BBF937D7B8F7503CFDCFF   ;  176
    #xFCB2CB35E702AF785CDA735244C3D43F   ;  177
    #x9DEFBF01B061ADAB3A0888136AFA64A8   ;  178
    #xC56BAEC21C7A1916088AAA1845B8FDD1   ;  179
    #xF6C69A72A3989F5B8AAD549E57273D46   ;  180
    #x9A3C2087A63F639936AC54E2F678864C   ;  181
    #xC0CB28A98FCF3C7F84576A1BB416A7DE   ;  182
    #xF0FDF2D3F3C30B9F656D44A2A11C51D6   ;  183
    #x969EB7C47859E7439F644AE5A4B1B326   ;  184
    #xBC4665B596706114873D5D9F0DDE1FEF   ;  185
    #xEB57FF22FC0C7959A90CB506D155A7EB   ;  186
    #x9316FF75DD87CBD809A7F12442D588F3   ;  187
    #xB7DCBF5354E9BECE0C11ED6D538AEB30   ;  188
    #xE5D3EF282A242E818F1668C8A86DA5FB   ;  189
    #x8FA475791A569D10F96E017D694487BD   ;  190
    #xB38D92D760EC445537C981DCC395A9AD   ;  191
    #xE070F78D3927556A85BBE253F47B1418   ;  192
    #x8C469AB843B8956293956D7478CCEC8F   ;  193
    #xAF58416654A6BABB387AC8D1970027B3   ;  194
    #xDB2E51BFE9D0696A06997B05FCC0319F   ;  195
    #x88FCF317F22241E2441FECE3BDF81F04   ;  196
    #xAB3C2FDDEEAAD25AD527E81CAD7626C4   ;  197
    #xD60B3BD56A5586F18A71E223D8D3B075   ;  198
    #x85C7056562757456F6872D5667844E4A   ;  199
    #xA738C6BEBB12D16CB428F8AC016561DC   ;  200
    #xD106F86E69D785C7E13336D701BEBA53   ;  201
    #x82A45B450226B39CECC0024661173474   ;  202
    #xA34D721642B0608427F002D7F95D0191   ;  203
    #xCC20CE9BD35C78A531EC038DF7B441F5   ;  204
    #xFF290242C83396CE7E67047175A15272   ;  205
    #x9F79A169BD203E410F0062C6E984D387   ;  206
    #xC75809C42C684DD152C07B78A3E60869   ;  207
    #xF92E0C3537826145A7709A56CCDF8A83   ;  208
    #x9BBCC7A142B17CCB88A66076400BB692   ;  209
    #xC2ABF989935DDBFE6ACFF893D00EA436   ;  210
    #xF356F7EBF83552FE0583F6B8C4124D44   ;  211
    #x98165AF37B2153DEC3727A337A8B704B   ;  212
    #xBE1BF1B059E9A8D6744F18C0592E4C5D   ;  213
    #xEDA2EE1C7064130C1162DEF06F79DF74   ;  214
    #x9485D4D1C63E8BE78ADDCB5645AC2BA9   ;  215
    #xB9A74A0637CE2EE16D953E2BD7173693   ;  216
    #xE8111C87C5C1BA99C8FA8DB6CCDD0438   ;  217
    #x910AB1D4DB9914A01D9C9892400A22A3   ;  218
    #xB54D5E4A127F59C82503BEB6D00CAB4C   ;  219
    #xE2A0B5DC971F303A2E44AE64840FD61E   ;  220
    #x8DA471A9DE737E245CEAECFED289E5D3   ;  221
    #xB10D8E1456105DAD7425A83E872C5F48   ;  222
    #xDD50F1996B947518D12F124E28F7771A   ;  223
    #x8A5296FFE33CC92F82BD6B70D99AAA70   ;  224
    #xACE73CBFDC0BFB7B636CC64D1001550C   ;  225
    #xD8210BEFD30EFA5A3C47F7E05401AA4F   ;  226
    #x8714A775E3E95C7865ACFAEC34810A72   ;  227
    #xA8D9D1535CE3B3967F1839A741A14D0E   ;  228
    #xD31045A8341CA07C1EDE48111209A051   ;  229
    #x83EA2B892091E44D934AED0AAB460433   ;  230
    #xA4E4B66B68B65D60F81DA84D56178540   ;  231
    #xCE1DE40642E3F4B936251260AB9D668F   ;  232
    #x80D2AE83E9CE78F3C1D72B7C6B42601A   ;  233
    #xA1075A24E4421730B24CF65B8612F820   ;  234
    #xC94930AE1D529CFCDEE033F26797B628   ;  235
    #xFB9B7CD9A4A7443C169840EF017DA3B2   ;  236
    #x9D412E0806E88AA58E1F289560EE864F   ;  237
    #xC491798A08A2AD4EF1A6F2BAB92A27E3   ;  238
    #xF5B5D7EC8ACB58A2AE10AF696774B1DC   ;  239
    #x9991A6F3D6BF1765ACCA6DA1E0A8EF2A   ;  240
    #xBFF610B0CC6EDD3F17FD090A58D32AF4   ;  241
    #xEFF394DCFF8A948EDDFC4B4CEF07F5B1   ;  242
    #x95F83D0A1FB69CD94ABDAF101564F98F   ;  243
    #xBB764C4CA7A4440F9D6D1AD41ABE37F2   ;  244
    #xEA53DF5FD18D551384C86189216DC5EE   ;  245
    #x92746B9BE2F8552C32FD3CF5B4E49BB5   ;  246
    #xB7118682DBB66A773FBC8C33221DC2A2   ;  247
    #xE4D5E82392A405150FABAF3FEAA5334B   ;  248
    #x8F05B1163BA6832D29CB4D87F2A7400F   ;  249
    #xB2C71D5BCA9023F8743E20E9EF511013   ;  250
    #xDF78E4B2BD342CF6914DA9246B255417   ;  251
    #x8BAB8EEFB6409C1A1AD089B6C2F7548F   ;  252
    #xAE9672ABA3D0C320A184AC2473B529B2   ;  253
    #xDA3C0F568CC4F3E8C9E5D72D90A2741F   ;  254
    #x8865899617FB18717E2FA67C7A658893   ;  255
    #xAA7EEBFB9DF9DE8DDDBB901B98FEEAB8   ;  256
    #xD51EA6FA85785631552A74227F3EA566   ;  257
    #x8533285C936B35DED53A88958F872760   ;  258
    #xA67FF273B84603568A892ABAF368F138   ;  259
    #xD01FEF10A657842C2D2B7569B0432D86   ;  260
    #x8213F56A67F6B29B9C3B29620E29FC74   ;  261
    #xA298F2C501F45F428349F3BA91B47B90   ;  262
    #xCB3F2F7642717713241C70A936219A74   ;  263
    #xFE0EFB53D30DD4D7ED238CD383AA0111   ;  264
    #x9EC95D1463E8A506F4363804324A40AB   ;  265
    #xC67BB4597CE2CE48B143C6053EDCD0D6   ;  266
    #xF81AA16FDC1B81DADD94B7868E94050B   ;  267
    #x9B10A4E5E9913128CA7CF2B4191C8327   ;  268
    #xC1D4CE1F63F57D72FD1C2F611F63A3F1   ;  269
    #xF24A01A73CF2DCCFBC633B39673C8CED   ;  270
    #x976E41088617CA01D5BE0503E085D814   ;  271
    #xBD49D14AA79DBC824B2D8644D8A74E19   ;  272
    #xEC9C459D51852BA2DDF8E7D60ED1219F   ;  273
    #x93E1AB8252F33B45CABB90E5C942B504   ;  274
    #xB8DA1662E7B00A173D6A751F3B936244   ;  275
    #xE7109BFBA19C0C9D0CC512670A783AD5   ;  276
    #x906A617D450187E227FB2B80668B24C6   ;  277
    #xB484F9DC9641E9DAB1F9F660802DEDF7   ;  278
    #xE1A63853BBD264515E7873F8A0396974   ;  279
    #x8D07E33455637EB2DB0B487B6423E1E9   ;  280
    #xB049DC016ABC5E5F91CE1A9A3D2CDA63   ;  281
    #xDC5C5301C56B75F77641A140CC7810FC   ;  282
    #x89B9B3E11B6329BAA9E904C87FCB0A9E   ;  283
    #xAC2820D9623BF429546345FA9FBDCD45   ;  284
    #xD732290FBACAF133A97C177947AD4096   ;  285
    #x867F59A9D4BED6C049ED8EABCCCC485E   ;  286
    #xA81F301449EE8C705C68F256BFFF5A75   ;  287
    #xD226FC195C6A2F8C73832EEC6FFF3112   ;  288
    #x83585D8FD9C25DB7C831FD53C5FF7EAC   ;  289
    #xA42E74F3D032F525BA3E7CA8B77F5E56   ;  290
    #xCD3A1230C43FB26F28CE1BD2E55F35EC   ;  291
    #x80444B5E7AA7CF857980D163CF5B81B4   ;  292
    #xA0555E361951C366D7E105BCC3326220   ;  293
    #xC86AB5C39FA634408DD9472BF3FEFAA8   ;  294
    #xFA856334878FC150B14F98F6F0FEB952   ;  295
    #x9C935E00D4B9D8D26ED1BF9A569F33D4   ;  296
    #xC3B8358109E84F070A862F80EC4700C9   ;  297
    #xF4A642E14C6262C8CD27BB612758C0FB   ;  298
    #x98E7E9CCCFBD7DBD8038D51CB897789D   ;  299
    #xBF21E44003ACDD2CE0470A63E6BD56C4   ;  300
    #xEEEA5D50049814781858CCFCE06CAC75   ;  301
    #x95527A5202DF0CCB0F37801E0C43EBC9   ;  302
    #xBAA718E68396CFFDD30560258F54E6BB   ;  303
    #xE950DF20247C83FD47C6B82EF32A206A   ;  304
    #x91D28B7416CDD27E4CDC331D57FA5442   ;  305
    #xB6472E511C81471DE0133FE4ADF8E953   ;  306
    #xE3D8F9E563A198E558180FDDD97723A7   ;  307
    #x8E679C2F5E44FF8F570F09EAA7EA7649   ;  308
    #xB201833B35D63F732CD2CC6551E513DB   ;  309
    #xDE81E40A034BCF4FF8077F7EA65E58D2   ;  310
    #x8B112E86420F6191FB04AFAF27FAF783   ;  311
    #xADD57A27D29339F679C5DB9AF1F9B564   ;  312
    #xD94AD8B1C738087418375281AE7822BD   ;  313
    #x87CEC76F1C8305488F2293910D0B15B6   ;  314
    #xA9C2794AE3A3C69AB2EB3875504DDB23   ;  315
    #xD433179D9C8CB8415FA60692A46151EC   ;  316
    #x849FEEC281D7F328DBC7C41BA6BCD334   ;  317
    #xA5C7EA73224DEFF312B9B522906C0801   ;  318
    #xCF39E50FEAE16BEFD768226B34870A01   ;  319
    #x81842F29F2CCE375E6A1158300D46641   ;  320
    #xA1E53AF46F801C5360495AE3C1097FD1   ;  321
    #xCA5E89B18B602368385BB19CB14BDFC5   ;  322
    #xFCF62C1DEE382C4246729E03DD9ED7B6   ;  323
    #x9E19DB92B4E31BA96C07A2C26A8346D2   ;  324
    #xC5A05277621BE293C7098B7305241886   ;  325
    #xF70867153AA2DB38B8CBEE4FC66D1EA8   ;  326
    #x9A65406D44A5C903737F74F1DC043329   ;  327
    #xC0FE908895CF3B44505F522E53053FF3   ;  328
    #xF13E34AABB430A15647726B9E7C68FF0   ;  329
    #x96C6E0EAB509E64D5ECA783430DC19F6   ;  330
    #xBC789925624C5FE0B67D16413D132073   ;  331
    #xEB96BF6EBADF77D8E41C5BD18C57E890   ;  332
    #x933E37A534CBAAE78E91B962F7B6F15A   ;  333
    #xB80DC58E81FE95A1723627BBB5A4ADB1   ;  334
    #xE61136F2227E3B09CEC3B1AAA30DD91D   ;  335
    #x8FCAC257558EE4E6213A4F0AA5E8A7B2   ;  336
    #xB3BD72ED2AF29E1FA988E2CD4F62D19E   ;  337
    #xE0ACCFA875AF45A793EB1B80A33B8606   ;  338
    #x8C6C01C9498D8B88BC72F130660533C4   ;  339
    #xAF87023B9BF0EE6AEB8FAD7C7F8680B5)) ;  340

(defconstant +expt10/min-exponent/64 -292)

(defconstant +expt10/max-exponent/64 340)

(defun integer-expt10/64 (power)
  (svref *expt10/values/64*
         (- (- +expt10/min-exponent/64) power)))

(defun floor-log2-expt10 (e)
  (ash (* e 1741647) -19))

(defun floor-log10-expt2 (e &optional three-quarters-p)
  (ash (- (* e 1262611)
          (if three-quarters-p 524031 0))
       -22))
