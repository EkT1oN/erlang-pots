-module(rss_parse).

-include_lib("xmerl/include/xmerl.hrl").

-export([
	is_rss2_feed/1, 
	get_feed_items/1, 
	get_item_time/1, 
	compare_feed_items/2
]).

%% @doc checks if Root is a valid rss2 feed
is_rss2_feed(Root) ->
	Root#xmlElement.name == rss
		andalso
		case lists:keyfind(version, #xmlAttribute.name, Root#xmlElement.attributes) of
			#xmlAttribute{value = "2.0"} -> true;
			_Else -> false
		end.

%% @doc collects all items from Root rss feed
get_feed_items(RSS2Feed) -> 
	xmerl_xpath:string("//item", RSS2Feed).

%% @doc extracts pubDate as a number of seconds since 0th year
get_item_time(Item) ->
	case lists:keyfind(pubDate, #xmlElement.name, Item#xmlElement.content) of
		#xmlElement{content = [#xmlText{value = PubDate}]} ->
		case httpd_util:convert_request_date(PubDate) of
			bad_date -> bad_date;
			Datetime -> calendar:datetime_to_gregorian_seconds(Datetime)
		end;
		false -> bad_date
	end.

%% @doc compares recency of OldItem and NewItem
%% returns same, if items are the same
%% returns updated, if NewItems is an updated version of the OldItem
%% returns different if OldItem and NewItem are different items
compare_feed_items(OldItem, NewItem) ->
	OldItemPure = extract_xml(OldItem),
	NewItemPure = extract_xml(NewItem),
	compare_feed_items_comb(
		OldItemPure,
		NewItemPure,
		[
			fun compare_feed_items_by_equality/2,
			fun compare_feed_items_by_guid/2,
			fun compare_feed_items_by_title/2,
			fun compare_feed_items_by_link/2
		]).

%% @doc checks recency of OldItem and NewItem using the list of comparators
%% propagates updated and same result
%% fallbacks on next comparator if current comparator returned different atom
compare_feed_items_comb(_, _, []) -> different;
compare_feed_items_comb(OldItem, NewItem, [H | Rest]) ->
	case H(OldItem, NewItem) of
		updated -> updated;
		same -> same;
		different -> compare_feed_items_comb(OldItem, NewItem, Rest)
	end.

%% @doc comparator to compare recency of OldItem and NewItem by equality
compare_feed_items_by_equality(OldItem, NewItem) when OldItem == NewItem -> 
	same;
compare_feed_items_by_equality(_, _) -> 
	different.

%% @doc compares recency of Left and Right items using Key element of content
compare_by_content(Left, Right, Key) ->
	LeftKey = lists:keyfind(Key, #xmlElement.name, Left#xmlElement.content),
	RightKey = lists:keyfind(Key, #xmlElement.name, Right#xmlElement.content),
	if
		(LeftKey /= false) and (RightKey /= false) ->
		if
			LeftKey == RightKey -> updated;
			true -> different
		end;
		true -> different
	end.

%% @doc compares recency of OldItem and NewItem using guid content element
compare_feed_items_by_guid(OldItem, NewItem) -> 
	compare_by_content(OldItem, NewItem, guid).

%% @doc compares recency of OldItem and NewItem using title content element
compare_feed_items_by_title(OldItem, NewItem) -> 
	compare_by_content(OldItem, NewItem, title).

%% @doc compares recency of OldItem and NewItem using link content element
compare_feed_items_by_link(OldItem, NewItem) -> 
	compare_by_content(OldItem, NewItem, link).

%% @doc extracts only necessary information from RSS elements
extract_xml(Elem = #xmlElement{}) ->
	Elem#xmlElement{parents = [], pos = 0,
		content = lists:map(fun extract_xml/1, Elem#xmlElement.content),
		attributes = lists:map(fun extract_xml/1, Elem#xmlElement.attributes)};
extract_xml(Attr = #xmlAttribute{}) ->
	Attr#xmlAttribute{parents = [], pos = 0};
extract_xml(Text = #xmlText{}) ->
	Text#xmlText{parents = [], pos = 0};
extract_xml(Comment = #xmlComment{}) ->
	Comment#xmlComment{parents = [], pos = 0};
extract_xml(Other) ->
	Other.
