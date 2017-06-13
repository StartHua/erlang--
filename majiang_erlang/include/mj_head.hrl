-define (MJ_WAN	,1).   	%万
-define (MJ_TIAO,2 ).   %条
-define (MJ_BING,3 ).   %饼
-define (MJ_FENG,4 ).   %东南西北(1357)
-define (MJ_ZFB ,5 ).   %中发白(135)
-record(r_userMyPai,{wan=[],tiao=[],bing=[],feng=[],zfb=[]}).

% -define(UserPai,
% 	[
% 		{my,    %chi,peng,gang,ting
% 			{mj_wan,[]},
% 			{mj_tiao,[]},
% 			{mj_bing,[]},
% 			{mj_feng,[]},
% 			{mj_zfb,[]}
% 		},
% 		{chi,    %chi,peng,gang,ting
% 			{mj_wan,[]},
% 			{mj_tiao,[]},
% 			{mj_bing,[]},
% 			{mj_feng,[]},
% 			{mj_zfb,[]}
% 		},
% 		{peng,    %chi,peng,gang,ting
% 			{mj_wan,[]},
% 			{mj_tiao,[]},
% 			{mj_bing,[]},
% 			{mj_feng,[]},
% 			{mj_zfb,[]}
% 		},
% 		{gang,    %chi,peng,gang,ting
% 			{mj_wan,[]},
% 			{mj_tiao,[]},
% 			{mj_bing,[]},
% 			{mj_feng,[]},
% 			{mj_zfb,[]}
% 		},
% 		{ting,    %chi,peng,gang,ting
% 			{mj_wan,[]},
% 			{mj_tiao,[]},
% 			{mj_bing,[]},
% 			{mj_feng,[]},
% 			{mj_zfb,[]}
% 		},
% 	]
% ).