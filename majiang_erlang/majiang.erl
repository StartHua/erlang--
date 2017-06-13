	% TempUserPai = #userPai_r{
	% 					my 		= #my_r{},
	% 					chi		= #chi_r{},
	% 					peng	= #peng_r{},
	% 					gang	= #gang_r{},
	% 					ting	= #ting_r{}
	% }

-module(majiang).

-include("mj_head.hrl").

-export([
	checkHu/1,
	test/0,
	getMyPaisForRecord/2, 
	checkSinglePaiType/1,	%%检查单张牌的类型，万饼筒条 123 --> 2
	checkSinglePaiNum/1 , 	%%检查单张牌的数值 123 ->3
	getPaiTypeNum/1			%%返回标准牌型数值(包括牌型与数字) 123 --> 12
	]).

test()->
	reloader:start(),
	% T1 =guangdong_majiang:checkSSY([11,12,13,22,22,22,23,24]),
	% T2 = guangdong_majiang:checkJLBD([11,12,13,22,22,22,23,24]),
	% checkHu([11,12,13,22,22,22,23,24]).
	T =checkHu([11,12,13,22,22]),

	% T =validABC([22,27,23],3),
	erlang:display(T).
	% erlang:display(T2).
	% erlang:display("======"),


checkHu(UserPai) ->	
	T = getMyPaisForRecord(UserPai,#r_userMyPai{}),
	%第一步检查AAA(减少检查，代码难看)
	{A1,B1} = validHu_AAA(T#r_userMyPai.wan,0),
	M = case (A1 and (B1 =< 1)) of
		true  ->
			{A2,B2} = validHu_AAA(T#r_userMyPai.tiao,0),
			case (A2 and (B2 =< 1)) of
				true  ->
					{A3,B3} = validHu_AAA(T#r_userMyPai.bing,0),
					case (A3 and (B3 =< 1)) of
						true  ->
							{A4,B4} = validHu_AAA(T#r_userMyPai.feng,0),
							case (A4 and (B4 =< 1)) of
								true  ->
									{A5,B5} = validHu_AAA(T#r_userMyPai.zfb,0),
									Count = B1+B2+B3+B4+B5,
									(A5 and (Count == 1));
								false ->
									false
							end;
						false ->
							false
					end;
				false ->
					false
			end;
		false ->	
			false
	end,

	% 第二部检查AA
	MAA = case M of
		true  ->
			true;
		false ->
			{A11,B11} = validHu_AA(T#r_userMyPai.wan,0),
			case (A11 and (B11 =< 1)) of
				true  ->
					{A22,B22} = validHu_AA(T#r_userMyPai.tiao,0),
					case (A22 and (B22 =< 1)) of
						true  ->
							{A33,B33} = validHu_AA(T#r_userMyPai.bing,0),
							case (A33 and (B33 =< 1)) of
								true  ->
									{A44,B44} = validHu_AA(T#r_userMyPai.feng,0),
									case (A44 and (B44 =< 1)) of
										true  ->
											{A55,B55} = validHu_AA(T#r_userMyPai.zfb,0),
											Count2 = B11+B22+B33+B44+B55,
											(A55 and (Count2 == 1));
										false ->
											false
									end;
								false ->
									false
							end;
						false ->
							false
					end;
				false ->	
					false
			end
		end,

		% 检查ABC
		MABC = case MAA of
			true ->
				true;
			false ->
				{A111,B111} = validHu_AA(T#r_userMyPai.wan,0),
				M111 = case (A111 and (B111 =< 1)) of
					true  ->
						{A222,B222} = validHu_AA(T#r_userMyPai.tiao,0),
						case (A222 and (B222 =< 1)) of
							true  ->
								{A333,B333} = validHu_AA(T#r_userMyPai.bing,0),
								case (A333 and (B333 =< 1)) of
									true  ->
										{A444,B444} = validHu_AA(T#r_userMyPai.feng,0),
										case (A444 and (B444 =< 1)) of
											true  ->
												{A555,B555} = validHu_AA(T#r_userMyPai.zfb,0),
												Count3 = B111+B222+B333+B444+B555,
												(A555 and (Count3 == 1));
											false ->
												false
										end;
									false ->
										false
								end;
							false ->
								false
						end;
					false ->	
						false
				end
			end.

%%检查单张牌的类型，万饼筒条 123 --> 2
checkSinglePaiType(Pai)->
	Pai rem 100 div 10.

%%检查单张牌的数值 123 ->3
checkSinglePaiNum(Pai)->
	Pai rem 10.

%%返回标准牌型数值(包括牌型与数字) 123 --> 12
getPaiTypeNum(Pai) ->
	Pai rem 100.

%%获取手牌r_userMyPai返回值
getMyPaisForRecord([],NPais)->
	%排序
	#r_userMyPai{
				   	wan 	= lists:sort(NPais#r_userMyPai.wan),
				   	tiao 	= lists:sort(NPais#r_userMyPai.tiao),
				   	bing 	= lists:sort(NPais#r_userMyPai.bing),
				   	feng 	= lists:sort(NPais#r_userMyPai.feng),
				   	zfb 	= lists:sort(NPais#r_userMyPai.zfb)
				};
getMyPaisForRecord([Pai | RemPais],NPais)->
	getMyPaisForRecord(RemPais,(
			case Pai div 100 of   % 检查是否符合
				0 ->
				   	case checkSinglePaiType(Pai) of
				   		?MJ_WAN ->
				   		  #r_userMyPai{
				   		  			   	wan= NPais#r_userMyPai.wan ++ [Pai],
				   		  			   	tiao= NPais#r_userMyPai.tiao,
				   		  			   	bing=NPais#r_userMyPai.bing,
				   		  			   	feng=NPais#r_userMyPai.feng,
				   		  			   	zfb=NPais#r_userMyPai.zfb
				   		  			};
				   		?MJ_TIAO ->
				   		  	#r_userMyPai{
				   		  			   	wan=NPais#r_userMyPai.wan,
				   		  			   	tiao=NPais#r_userMyPai.tiao ++ [Pai],
				   		  			   	bing=NPais#r_userMyPai.bing,
				   		  			   	feng=NPais#r_userMyPai.feng,
				   		  			   	zfb=NPais#r_userMyPai.zfb
				   		  			};
				   		?MJ_BING ->
				   		  	#r_userMyPai{
				   		  			   	wan=NPais#r_userMyPai.wan,
				   		  			   	tiao=NPais#r_userMyPai.tiao,
				   		  			   	bing=NPais#r_userMyPai.bing ++ [Pai],
				   		  			   	feng=NPais#r_userMyPai.feng,
				   		  			   	zfb=NPais#r_userMyPai.zfb
				   		  			};
				   		?MJ_FENG ->
				   			#r_userMyPai{
				   		  			   	wan=NPais#r_userMyPai.wan,
				   		  			   	tiao=NPais#r_userMyPai.tiao,
				   		  			   	bing=NPais#r_userMyPai.bing ,
				   		  			   	feng=NPais#r_userMyPai.feng++ [Pai],
				   		  			   	zfb=NPais#r_userMyPai.zfb
				   		  			};
				   		?MJ_ZFB ->
				   			#r_userMyPai{
				   		  			   	wan=NPais#r_userMyPai.wan,
				   		  			   	tiao=NPais#r_userMyPai.tiao,
				   		  			   	bing=NPais#r_userMyPai.bing ,
				   		  			   	feng=NPais#r_userMyPai.feng,
				   		  			   	zfb=NPais#r_userMyPai.zfb++ [Pai]
				   		  			};
				   		Other ->
				   			NPais
				   	end;
				Other->
					NPais
			end
		)).

%获取r_userMyPai 里面的长度
getRecordMypaisLength(R_UserPais)  ->
	length(R_UserPais#r_userMyPai.wan)+
	length(R_UserPais#r_userMyPai.tiao)+
	length(R_UserPais#r_userMyPai.bing)+
	length(R_UserPais#r_userMyPai.feng)+
	length(R_UserPais#r_userMyPai.zfb).




% =================================普通胡牌================================
% --测试胡n张牌
% --IN：用户牌，检测起点，检测总数
% --OUT：是否胡牌，将牌数
validHu_AAA(Pais,Count)->
	N = length(Pais),
	case (N == 0)  of   %空牌直接胡牌
		true  ->
			{true,Count};
		_->
			case (N rem 3) == 1 of % 存在两个将牌或少于两个牌，不可能胡
				true ->
					{false,Count};
				_->
					{T,RPais} = validAAA(Pais,N),
					case T of  %检查AAA牌
						true->
							validHu_AAA(RPais,Count);
						false->
							{T1 , RPais2} = validAA(Pais,N),
							case T1 of %检查AA牌
							 	true ->	
							 		validHu_AAA(RPais2,Count+1);
							 	false ->
							 		% 对万，条，饼测试ABC
							 		[SinglePai | Rem] = Pais,
							 		case (checkSinglePaiType(SinglePai) /= ?MJ_FENG) and (checkSinglePaiType(SinglePai) /= ?MJ_ZFB) of
							 			true->
							 				{T2,NewPai} = validABC(Pais,N),
							 				case T2 of
							 						true-> 
							 							validHu_AAA(NewPai,Count);
							 						false-> 
							 							{false,Count}
							 						end;
							 			false->
							 				{false,Count}
							 		end
							 end 
					end
			end
	end.

validHu_AA(Pais,Count)->
	N = length(Pais),
	case (N == 0)  of   %空牌直接胡牌
		true  ->
			{true,Count};
		_->
			case (N rem 3) == 1 of % 存在两个将牌或少于两个牌，不可能胡
				true ->
					{false,Count};
				_->
					{T,RPais} = validAA(Pais,N),
					
					case T of  %检查AAA牌
						true->
							validHu_AA(RPais,Count+1);
						false->
							{T1 , RPais2} = validAAA(Pais,N),
							case T1 of %检查AA牌
							 	true ->	
							 		validHu_AA(RPais2,Count);
							 	false ->
							 		% 对万，条，饼测试ABC
							 		[SinglePai | Rem] = Pais,
							 		case (checkSinglePaiType(SinglePai) /= ?MJ_FENG) and (checkSinglePaiType(SinglePai) /= ?MJ_ZFB) of
							 			true->
							 				{T2,NewPai} = validABC(Pais,N),
							 				case T2 of
							 						true-> 
							 							validHu_AA(NewPai,Count);
							 						false-> 
							 							{false,Count}
							 						end;
							 			false->
							 				{false,Count}
							 		end
							 end 
					end
			end
	end.

validHu_ABC(Pais,Count)->
	N = length(Pais),
	case (N == 0)  of   %空牌直接胡牌
		true  ->
			{true,Count};
		_->
			case (N rem 3) == 1 of % 存在两个将牌或少于两个牌，不可能胡
				true ->
					{false,Count};
				_->
					% 对万，条，饼测试ABC
					[SinglePai | Rem] = Pais,
			 		{TT,NP} = case (checkSinglePaiType(SinglePai) /= ?MJ_FENG) and (checkSinglePaiType(SinglePai) /= ?MJ_ZFB) of
						true->
							{T2,NewPai} = validABC(Pais,N),
							 	case T2 of
							 		true-> 
										{true,NewPai};
			 						false-> 
			 							{false,Count}
		 						end;
		 	 			false->
			 				{false,Count}
				 	end,

				 	case TT of
				 		true  ->
				 			validHu_ABC(NP,Count);
				 		false ->
				 			{T,RPais} = validAAA(Pais,N),
								case T of  %检查AAA牌
									true->
										validHu_ABC(RPais,Count);
									false->
										{T1 , RPais2} = validAA(Pais,N),
										case T1 of %检查AA牌
							 				true ->	
							 					validHu_ABC(RPais2,Count+1);
							 				false ->
							 					{false,Count}
							 			end 
								end
				 	end	
			end
	end.
% =================================普通胡牌================================
% 测试胡AAA牌（刻子）
validAAA(Pais,N)->
	case N >=  3  of
		true  ->
			validAAA_(Pais,N);	
		false ->
			{false,Pais}
	end.
validAAA_([A,B,C | D],N)->
	case ((A == B) and (A == C)) of
		true  ->
			{true,D};
		false ->
			{false,[A]++[B]++[C]++D}
	end.


% 测试胡AAA牌（刻子）
validAA(Pais,N)->
	case N >= 2  of
		true  ->
			validAA_(Pais,N);	
		false ->
			{false,Pais}
	end.
validAA_([A,B | C],N)->
	case (A==B) of
		true  ->
			{true,C};
		false ->
			{false,[A]++[B]++C}
	end.


% 测试胡ABC牌（顺子）
validABC(Pais,N)->
	case N >= 3 of
		false  ->
			{false,Pais};
		true ->
			[A | B] = Pais,
			 {T,L}= validABC_1(A,B,[]),			
			 case T of
			 	true ->
			 		[A1,B1 | C] = L,
			 		{T2,L2}  = validABC_2([A1,B1],C,[]),			 		
			 		case T2 of
			 			true ->
			 				[N1,N2,N3 | N4] =L2,
			 				{true,N4};
			 			false ->
			 				{false,L2}
			 		end;
			 	false ->
			 		{false,Pais}
			 end
	end.
%调换第一个位置[1,3,2,5,4] -->[1,2,3,5,4]
validABC_1(A,[],D)->
	{false,[A]++D};
validABC_1(A,[B | C],D)->
	case B == (A + 1) of
		 true ->
		 	case D ==[] of
		 		true->
		 			T1 = [A]++[B]++C,
		 			{true,T1};
		 		false->
		 			[E | F] = D,
					T2 = [A]++[B]++F++[E]++C,
					{true,T2}
		 	end;
		false->
			K = D++[B],
			validABC_1(A,C,K)
	end.
%调换第二个位置[1,2,2,2,3] -->[1,2,3,2,2]
validABC_2([A,B],[],E)->
	{false,[A,B]++E};
validABC_2([A,B],[C | D],E)->
	case C == (A + 2) of
		true ->
			case E == [] of
				true ->
					T = [A,B]++[C] ++D,
					{true,T};
				false->
					[F | G] = E,
					T = [A,B]++[C]++G++[F]++D,
					{true,T}
			end;
		false->
			K = E ++ [C],
			validABC_2([A,B],D,K)
	end.


%获取牌的牌型和牌号码列表
getJLBD([],N)->
	N;
getJLBD([A | B],N)->
	getJLBD(B,N++[getPaiTypeNum(A)]).
























