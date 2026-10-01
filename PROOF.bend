import Base
import ./LAWS.bend as Laws
import ./core.bend as C
import ./twitch/logic.bend as T

def append_assoc(a: String, b: String, c: String) -> {String.append(String.append(a, b), c) == String.append(a, String.append(b, c)) : String}:
  match a:
    case SNil{}:
      {==}
    case SCon{h, t}:
      %append_assoc(t, b, c) : {SCon{h, String.append(String.append(t, b), c)} == SCon{h, _} : String}
      {==}

def Laws.esc_append(a, b):
  match a:
    case SNil{}:
      {==}
    case SCon{+h, +t}:
      %Equal.sym(String, C.esc(String.append(t, b)), String.append(C.esc(t), C.esc(b)), Laws.esc_append(t, b)) : {String.append(C.esc.char(h), _) == String.append(String.append(C.esc.char(h), C.esc(t)), C.esc(b)) : String}
      Equal.sym(String, String.append(String.append(C.esc.char(h), C.esc(t)), C.esc(b)), String.append(C.esc.char(h), String.append(C.esc(t), C.esc(b))), append_assoc(C.esc.char(h), C.esc(t), C.esc(b)))

def Laws.tw_token_prefix(t):
  match t:
    case SNil{}:
      {==}
    case SCon{h, r}:
      {==}


def Laws.as_line_chat(id, p, ch, from, body, badges, reply):
  {==}

def Laws.as_line_tip():
  {==}

def Laws.as_line_sub():
  {==}

def Laws.event_json_sub():
  {==}

def Laws.event_json_gift():
  {==}

def Laws.event_json_raid():
  {==}

def Laws.as_line_deleted(target, body):
  {==}

def Laws.as_line_custom():
  {==}

def Laws.event_json_tip():
  {==}

def Laws.event_json_custom():
  {==}

def Laws.event_json_banned():
  {==}

def Laws.digits_big():
  {==}

def Laws.esc_control():
  {==}

def Laws.esc_utf8():
  {==}

def Laws.wire_line():
  {==}

def Laws.lines_split():
  {==}

def Laws.head_split():
  {==}

def Laws.head_waits():
  {==}

def Laws.ws_key():
  {==}

def Laws.url_esc():
  {==}

def Laws.byo_reads():
  {==}

def Laws.byo_only_its_table():
  {==}

def Laws.config_get():
  {==}

def Laws.opts_flags():
  {==}

def Laws.opts_bad_port():
  {==}

def Laws.opts_no_platform_flags():
  {==}

def Laws.req_say():
  {==}

def Laws.req_any_platform():
  {==}

def Laws.req_no_platform():
  {==}

def Laws.req_empty():
  {==}

def Laws.req_line_break():
  {==}

def Laws.req_not_say():
  {==}

def Laws.req_delete():
  {==}

def Laws.req_delete_no_id():
  {==}

def Laws.reply_deleted():
  {==}

def Laws.asked_say():
  {==}

def Laws.asked_say_everywhere(chs):
  {==}

def Laws.asked_say_one():
  {==}

def Laws.asked_say_line_break():
  {==}

def Laws.asked_delete():
  {==}

def Laws.asked_heartbeat():
  {==}

def Laws.notice_deleted():
  {==}

def Laws.notice_said(id):
  {==}

def Laws.notice_said_failed():
  {==}

def Laws.frames_chat(body):
  {==}

def Laws.frames_sub():
  {==}

def Laws.frames_tip():
  {==}

def Laws.frames_deleted(body):
  {==}

def Laws.targets_all(names):
  {==}

def Laws.targets_one():
  {==}

def Laws.utf8_length():
  {==}

def Laws.said_line():
  {==}

def Laws.reply_refused():
  {==}

def Laws.api_failed_google():
  {==}

def Laws.api_failed_twitch():
  {==}

def Laws.tw_privmsg(body):
  {==}

def Laws.tw_untagged(body):
  {==}

def Laws.tw_tag_escapes():
  {==}

def Laws.tw_ping(token):
  {==}

def Laws.tw_join_ignored():
  {==}

def Laws.tw_cheer():
  {==}

def Laws.tw_resub(body):
  {==}

def Laws.tw_sub_silent():
  {==}

def Laws.tw_gift():
  {==}

def Laws.tw_community_gift():
  {==}

def Laws.tw_community_gift_parts():
  {==}

def Laws.tw_raid():
  {==}

def Laws.tw_announcement():
  {==}

def Laws.tw_notice_without_id():
  {==}

def Laws.tw_channel():
  {==}

def Laws.tw_token_plain():
  {==}

def Laws.tw_me():
  {==}

def Laws.tw_me_invalid():
  {==}

def Laws.tw_need():
  {==}

def Laws.tw_delete_url():
  {==}

def Laws.tw_body():
  {==}

def Laws.tw_sent():
  {==}

def Laws.tw_dropped():
  {==}

def Laws.tw_badges_reply():
  {==}

def Laws.tw_redemption():
  {==}

def Laws.tw_power_up():
  {==}

def Laws.tw_gift_upgrade():
  {==}

def Laws.tw_deleted(body):
  {==}

def Laws.tw_timeout():
  {==}

def Laws.tw_ban():
  {==}

def Laws.tw_cleared():
  {==}

def Laws.yt_badges():
  {==}

def Laws.yt_deleted():
  {==}

def Laws.yt_timeout():
  {==}

def Laws.yt_ban():
  {==}

def Laws.yt_own_kind():
  {==}

def Laws.yt_page():
  {==}

def Laws.yt_superchat():
  {==}

def Laws.yt_member():
  {==}

def Laws.yt_milestone():
  {==}

def Laws.yt_gifting():
  {==}

def Laws.yt_chat_id():
  {==}

def Laws.yt_refresh_form():
  {==}

def Laws.yt_token():
  {==}

def Laws.yt_token_refused():
  {==}

def Laws.yt_delete_url():
  {==}

def Laws.yt_body():
  {==}

def Laws.tw_renewed():
  {==}

def Laws.tw_renewed_keeps_refresh(old):
  {==}

def Laws.tw_renewed_refused():
  {==}

def Laws.tw_renew_form():
  {==}

def Laws.es_welcome():
  {==}

def Laws.es_keepalive():
  {==}

def Laws.es_reconnect():
  {==}

def Laws.es_revoked():
  {==}

def Laws.es_follow():
  {==}

def Laws.es_redemption():
  {==}

def Laws.es_redemption_with_text():
  {==}

def Laws.es_poll_end():
  {==}

def Laws.es_prediction_end():
  {==}

def Laws.es_stream_online():
  {==}

def Laws.es_raid_out():
  {==}

def Laws.es_plan():
  {==}

def Laws.es_body():
  {==}

def Laws.es_ws_url():
  {==}
