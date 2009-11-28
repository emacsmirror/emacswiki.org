;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is mon-iptables-vars.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-iptables-vars.el provides variable `*mon-iptables-alst*' which
;;; needs to be evaluated before :FILE `mon-iptables-regexps.el'.
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS:
;;; `*mon-iptables-alst*'
;;;
;;; VARIABLES:
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;;
;;; TODO:
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; REQUIRES:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-iptables-vars.el')
;;; FIRST-PUBLISHED: 
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-11-27T22:53:53-05:00Z}#{09485} - by MON KEY>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Permission is granted to copy, distribute and/or modify this
;;; document under the terms of the GNU Free Documentation License,
;;; Version 1.3 or any later version published by the Free Software
;;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;;; and no Back-Cover Texts. A copy of the license is included in
;;; the section entitled "GNU Free Documentation License".
;;; A copy of the license is also available from the Free Software
;;; Foundation Web site at:
;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ================================================================
;;; Copyright © 2009 MON KEY 
;;; ==============================
;;; CODE:

;;; ==============================
;;; :CREATED <Timestamp: #{2009-11-25T18:33:44-05:00Z}#{09483} - by MON>
(defconst *mon-iptables-alst*
  '((:IPTABLES-TARGETS .
     (("ACCEPT" "DROP" "QUEUE" "RETURN")))
    (:IPTABLES-TABLES .
     (("--table"                        ;table
       ("filter"                        ;Default table.
        ("OUTPUT" "INPUT" "FORWARD"))
       ("nat"                      ;Consulted when a packet makes new connection
        ("PREROUTING" "OUTPUT" "POSTROUTING"))
       ("mangle"                        ;Used for packet alteration.
        ("PREROUTING" "OUTPUT" "INPUT" "FORWARD" "POSTROUTING"))
       ("raw"                        ; Configure connection tracking exemptions.
        ("PREROUTING" "OUTPUT")))))
    (:IPTABLES-COMMANDS .
     (("--append"       . "-A")         ;chain rule-specification
      ("--delete"        . "-D")        ;chain rule-specification|chain rulenum
      ("--rename-chain"  . "-E")        ;old-chain new-chain
      ("--flush"         . "-F")        ;[chain]
      ("--insert"        . "-I")        ;chain [rulenum] rule-specification
      ("--policy"        . "-P")        ;chain target
      ("--new-chain"     . "-N")        ;chain
      ("--list"          . "-L")        ;[chain]
      ("--replace"       . "-R")        ;chain rulenum rule-specification
      ("--list-rules"    . "-S")        ;[chain]
      ("--delete-chain"  . "-X")        ;[chain]
      ("--zero"          . "-Z")        ;[chain]
      ))

    (:IPTABLES-PARAMETERS .
     (("--set-counters"  . "-c")        ;packets bytes
      ("--destination"   . "-d")        ;address[/mask]  "--dst" <- :ALIASED-BY
      ("--fragment"      . "-f")        ;
      ("--goto"          . "-g")        ;chain
      ("--in-interface"  . "-i")        ;name
      ("--jump"          . "-j")        ;target
      ("--match"         . "-m")        ;matchname [per-match-options]
      ("--out-interface" . "-o")        ;name
      ("--protocol"      . "-p") ;protocol {"tcp" "udp" "udplite" "icmp" "esp" "ah" "sctp" "all"}
                                        ;"--proto" <- :ALIASED-BY
      ("--source"        . "-s")        ;; address[/mask] "--src" <- :ALIASED-BY
      ))

    (:IPTABLES-OPTIONS .
     (("--help"    . "-h")
      ("--numeric" . "-n")
      ("--verbose" . "-v")
      ("--exact"   . "-x")
      ("--line-numbers")
      ("--modprobe")                    ;=command
      ))

    (:IPTABLES-MATCH-EXTENSIONS .
     (("addrtyp" .
                 ("UNSPEC"              ;unspecified address (i.e. 0.0.0.0)
                  "UNICAST"             ;unicast address
                  "LOCAL"               ;local address
                  "BROADCAST"           ;broadcast address
                  "ANYCAST"             ;anycast packet
                  "MULTICAST"           ;multicast address
                  "BLACKHOLE"           ;blackhole address
                  "UNREACHABLE"         ;unreachable address
                  "PROHIBIT"            ;prohibited address
                  ("XRESOLVE" .
                              ("--src-type"     ;type
                               "--dst-type"     ;type
                               "--limit-iface-in" ;interface <DEVICE> e.g. eth0
                               "--limit-iface-out" ;interface <DEVICE>
                               ))))
      ("ah" .                           ;SPIs in Authentication Header
            ("--ahspi"                  ;spi[:spi]
             ))
      ("comment" .
                 ("--comment"           ;comment <STRING>
                  ))
      ("connbytes" .
                   ("--connbytes"       ;from[:to]
                    "--connbytes-dir"   ;{original|reply|both}
                    "--connbytes-mode"  ;{packets|bytes|avgpkt}
                    ))
      ("connlimit" .
                   ("--connlimit-above" ;n  <WHOLENUM>
                    "--connlimit-mask"  ;; prefix_length <WHOLENUM>
                    ))
      ;; IPv4 0-32 IPv6 0-128
      ("connmark" .
                  ("--mark"             ;value[/mask]
                   ))
      ("conntrack" .
                   ("--ctproto"         ;l4proto
                    "--ctorigsrc"       ;address[/mask]
                    "--ctorigdst"       ;address[/mask]
                    "--ctreplsrc"       ;address[/mask]
                    "--ctrepldst"       ;address[/mask]
                    "--ctorigsrcport"   ;port
                    "--ctorigdstport"   ;port
                    "--ctreplsrcport"   ;port
                    "--ctrepldstport"   ;port
                    "--ctexpire"        ;time[:time]
                    "--ctdir"           ;{ORIGINAL|REPLY}
                    ("--ctstate" .      ;; statelist
                                 ("INVALID" "NEW" "ESTABLISHED" "RELATED" "SNAT" "DNAT"))
                    ("--ctstatus" . ;; statelist
                                  ("NONE" "EXPECTED" "SEEN_REPLY" "ASSURED" "CONFIRMED"))))
      ("dccp" .
              ("--source-port"          ;port[:port]
               "--sport"                ;port[:port]
               "--destination-port"     ;port[:port]
               "--dport"                ;port[:port]
               "--dccp-option"          ;<WHOLENUM>
               ("--dccp-types" .        ;mask
                               ("REQUEST" "RESPONSE" "DATA" "ACK" "DATAACK"
                                          "CLOSEREQ" "CLOSE" "RESET" "SYNC" "SYNCACK"
                                          "INVALID"))))
      ("dscp" .                         ;6 bit DSCP field in TOS field of header
              (("--dscp value"          ;decimal or hex value [0-63]
                ("--dscp-class" .       ;class
                                ("BE" "EF" "AFxx" "CSx")))))
      ("ecn" .                   ;ECN bits of IPv4 and TCP header :SEE [RFC3168]
             ("--ecn-ip-ect"     ;num 0-3
              ("--ecn-tcp-cwr" .
                               ("TCP" "ECN" "CWR"))
              ("--ecn-tcp-ece" .
                               ("TCP" "ECN" "ECE") ;`ECN' -> ECHO Echo bit
                               )))
      ("esp" .
             ("--espspi"                ;spi[:spi]
              ))
      ("hashlimit" .
                   ("--hashlimit-htable-expire"   ;msec
                    "--hashlimit-htable-gcinterval" ;msec
                    "--hashlimit-upto" ;amount[/second|/minute|/hour|/day] <WHOLENUM/QUANTA>
                    "--hashlimit-above" ;amount[/second|/minute|/hour|/day] <WHOLENUM/QUANTA>
                    "--hashlimit-burst" ;amount <WHOLENUM>
                    "--hashlimit-mode" ;{srcip|srcport|dstip|dstport} <CSV-LIST>
                    "--hashlimit-srcmask"   ;prefix 0-32 <WHOLENUM>
                    "--hashlimit-dstmask"   ;prefix 0-32 <WHOLENUM>
                    "--hashlimit-name"      ;foo <NAME>
                    "--hashlimit-htable-size" ;buckets <WHOLENUM>
                    "--hashlimit-htable-max"  ;entries"  <WHOLENUM>
                    ))
      ("helper" .
                ("--helper"             ;string
                 ))
      ("icmp" .
              ("--icmp-type"            ;type[/code]|typename}
               ))
      ("iprange" .
                 ("--src-range"         ;from[-to]
                  "--dst-range"         ;from[-to]
                  ))
      ("length" . ("--length"           ;length[:length]
                   ))
      ("limit"  .
                ("--limit"              ;rate[/second|/minute|/hour|/day]
                 "--limit-burst"        ;number
                 ))
      ("mac"   .                        ;MAC address
               ("--mac-source"          ;address <XX:XX:XX:XX:XX:XX>
                ))
      ("mark"  .
               ("--mark"                ;value[/mask]
                ))
      ("multiport" .
                   ("--source-ports"      ;port[,port|,port:port]
                    "--sports"            ;port[,port|,port:port]
                    "--destination-ports" ;port[,port|,port:port]
                    "--dports"            ;port[,port|,port:port]
                    "--ports"             ;port[,port|,port:port]
                    ))
      ("owner" .
               ("--uid-owner"           ;username
                "--uid-owner"           ;userid[-userid]
                "--gid-owner"           ;groupname
                "--gid-owner"           ;groupid[-groupid]
                "--socket-exists"))
      ("physdev" .
                 ("--physdev-in"        ;name
                  "--physdev-out"       ;name
                  "--physdev-is-in"
                  "--physdev-is-out"
                  "--physdev-is-bridged"))
      ("pkttype" .
                 ("--pkt-type"          ;{unicast|broadcast|multicast}
                  ))
      ("policy" .
                ("--dir"                ;{in|out}
                 "--pol"                ;{none|ipsec}
                 "--strict"
                 "--reqid"              ;id
                 "--spi"                ;spi
                 "--proto"              ;{ah|esp|ipcomp}
                 "--mode"               ;{tunnel|transport}
                 "--tunnel-src"         ;addr[/mask]
                 "--tunnel-dst"         ;addr[/mask]
                 "--next"))
      ("rateest" .                      ; Rate estimator
                 ("--rateest1"          ;name
                  "--rateest2"          ;name
                  "--rateest-delta"
                  "--rateest1-bps"      ;value
                  "--rateest2-bps"      ;value
                  "--rateest1-pps"      ;value
                  "--rateest2-pps"      ;value
                  "--rateest-lt"
                  "--rateest-gt"
                  "--rateest-eq"))
      ("realm" .
               ("--realm"               ;value[/mask]
                ))
      ("recent" .       ;; :SEE (URL `http://snowman.net/projects/ipt_recent/')
                ("--name"               ;name
                 "--set"
                 "--rsource"
                 "--rdest"
                 "--rcheck"
                 "--update"
                 "--remove"
                 "--seconds"            ;seconds
                 "--hitcount"           ;hits
                 "--rttl"))
      ("sctp" .
              ("--source-port"          ;port[:port]
               "--sport"                ;port[:port]
               "--destination-port"     ;port[:port]
               "--dport"                ;port[:port]
               ("--chunk-types" .       ; {all|any|only} chunktype[:flags] [...]
                                ;; :CHUNK-TYPES
                                ("INIT" "INIT_ACK" "SACK" "HEARTBEAT" "HEARTBEAT_ACK"
                                        "SHUTDOWN_ACK" "ERROR" "COOKIE_ECHO" "COOKIE_ACK"
                                        "ECN_ECNE" "ECN_CWR" "SHUTDOWN" "ASCONF" "ASCONF_ACK"
                                        ("DATA" . ;; chunk-type w/ available flags
                                                ("U" ;match if set
                                                 "B" ;match if set
                                                 "E" ;match if set
                                                 "u" ;match if unset
                                                 "b" ;match if unset
                                                 "e" ;match if unset
                                                 ))
                                        ("ABORT" . ;; chunk-type w/ available flags
                                                 ("T" ;match if set
                                                  "t" ;match if unset
                                                  ))
                                        ("SHUTDOWN_COMPLETE" . ;; chunk-type w/ available flags
                                                             ("T" ;match if set
                                                              "t" ;match if unset
                                                              ))))))
      ("set" .                          ; IP sets definable by `ipset'
             ("--set"                   ;setname flag[,flag]...
              ))
      ("socket")                        ;match open socket for packet
      ("state" .                        ;state connection tracking.
               ("--state" .             ;state <CSV-LIST>
                          ("INVALID"
                           "ESTABLISHED"
                           "NEW"
                           "RELATED")))
      ("statistic" .                ;match packets based on statistcal condition
                   ("--mode"        ;mode - [random|nth]
                    "--probability" ;p [0 to 1]
                    "--every"       ;n
                    "--packet"      ;p
                    ))
      ("string" .         ; match string using pattern
                ("--algo" ;{bm|kmp} bm -> Boyer-Moore, kmp -> Knuth-Pratt-Morris
                 "--from" ;offset
                 "--to"   ;offset
                 "--string"             ;pattern
                 "--hex-string"         ;pattern
                 ))
      ("tcp" .                          ;Match if `--protocol tcp' is specified
             ("--source-port"           ;port[:port]
              "--sport"                 ;port[:port]
              "--destination-port"      ;port[,port]
              "--dport"                 ;port[,port]
              "--syn"                   ; if SYN set and ACD, RST, FIN clear
              "--tcp-option"            ;number
              ("--tcp-flags" .          ;mask comp
                             ;;comp is a  <CSV-LIST> of:
                             ("SYN" "ACK" "FIN"  "RST" "URG" "PSH" "ALL" "NONE")
                             ;;  _______________________________________
                             ;; |                                       |
                             ;; |     URG       ->       Urgent         |
                             ;; |     ACK       ->       Acknowledge    |
                             ;; |     PSH       ->       Push/Flush     |
                             ;; |     RST       ->       Reset          |
                             ;; |     SYN       ->       Synchronize    |
                             ;; |     FIN       ->       Finish         |
                             ;; |_______________________________________|
                             )))

      ("tcpmss" .      ;match TCP MSS (maximum segment size) field of TCP header
                ("--mss"))              ;value[:value]
      ("time" .                         ;match packet arrival time w/in range
              ("--datestart"            ;YYYY[-MM[-DD[Thh[:mm[:ss]]]]]
               "--datestop"             ;YYYY[-MM[-DD[Thh[:mm[:ss]]]]]
               "--timestart"            ;hh:mm[:ss]
               "--timestop"             ;hh:mm[:ss]
               "--monthdays"            ;day[,day...]
               "--weekdays"             ;day[,day...]
               "--utc"
               "--localtz"))
      ("tos" .                          ;match 8-bit Type of Service
             ("--tos"                   ;value[/mask]
              "--tos"))                 ;symbol
      ("ttl" .                          ;match time to live in IP header
             ("--ttl-eq"                ;ttl
              "--ttl-gt"                ;ttl
              "--ttl-lt"))              ;ttl
      ("u32" .         ;Test for value of bytes (up to 4) extracted from packet.
             ("--u32"  ;tests
              ;;,----
              ;;| tests :=  location "=" value | tests "&&" location "=" value
              ;;| value := range | value "," range
              ;;| range := number | number ":" number
              ;;| location := number | location operator number
              ;;| operator := "&" | "<<" | ">>" | "@"
              ;;`----
              ))
      ("udp" .                         ; Match if `--protocol udp' is specified.
             ("--source-port"          ;port[:port]
              "--sport"                ;port[:port]
              "--destination-port"     ;port[:port]
              "--dport"                ;port[:port]
              ))
      ("unclean")))

    (:IPTABLES-TARGET-EXTENSIONS .
     (("CLASSIFY" .              ; Classify the packet into a specific CBQ class
                  ("--set-class" ;major:minor
                   ))
      ("CLUSTERIP" .               ; Cluster nodes sharing an IP and MAC address
                   ("--new"
                    "--hashmode"        ;mode
                    "--clustermac"      ;mac
                    "--total-nodes"     ;num
                    "--local-node"      ;num
                    "--hash-init"       ;rnd
                    ))
      ("CONNMARK" .
                  (("--save-mark" .         ;[--nfmask nfmask] [--ctmask ctmask]
                                  ("--nfmask" ;nfmask
                                   "--ctmask" ;ctmask
                                   ))
                   ("--restore-mark" .  ;[--nfmask nfmask] [--ctmask ctmask]
                                     ("--nfmask" ;nfmask
                                      "--ctmask" ;ctmask
                                      ))
                   ("--set-xmark"  .                       ;value[/mask]
                                   ("--and-mark"           ;bits
                                    "--or-mark"            ;bits
                                    "--xor-mark"           ;bits
                                    "--set-mark"           ;value[/mask]
                                    ("--save-mark" .       ;[--mask mask]
                                                   ("--mask" ;mask
                                                    ))
                                    ("--restore-mark" .       ;[--mask mask]
                                                      ("--mask" ;mask
                                                       ))))))
      ("CONNSECMARK" .                  ;Copy security  markings  from  packet.
                     ("--save"
                      "--restore"))
      ("DNAT" .             ; Specify destination address modifificat of packet.
              ("--to-destination"       ;[ipaddr][-ipaddr][:port[-port]]
               "--random"))
      ("DSCP" .             ;Alter DSCP bit values in TOS header of IPv4 packet.
              ("--set-dscp" ;value (decimal or hex)
               "--set-dscp-class"       ;class
               ))
      ("ECN" .                    ;Selectively work around known ECN blackholes.
             ("--ecn-tcp-remove"))
      ("LOG" .                          ; Kernel logging of matching packets
             ("--log-level"             ;level
              "--log-prefix"            ;prefix
              "--log-tcp-sequence"
              "--log-tcp-options"
              "--log-ip-options"
              "--log-uid"))
      ("MARK" .                           ; Set packet's Netfilter mark value.
              ("--set-mark"               ;value[/mask]
               ("--set-xmark" .           ;value[/mask]
                              ("--and-mark" ;bits
                               "--or-mark"  ;bits
                               "--xor-mark" ;bits
                               ))))
      ("MASQUERADE" .
                    ("--to-ports"       ;port[-port]
                     "--random"))
      ("MIRROR" ;Invert source and destination fields in IP header and retransmit.
       )
      ("NETMAP" . ;Statically map network addresses to another network of addresses.
                ("--to address"         ;[/mask]
                 ))
      ("NFLOG" .                        ;Log matching packets.
               ("--nflog-group"         ;nlgroup
                "--nflog-prefix"        ;prefix
                "--nflog-range"         ;size
                "--nflog-threshold"     ;size
                ))
      ("NFQUEUE"  .      ;Put packet in specific queue with 16-bit queue number.
                  ("--queue-num"        ;value  (0 to 65535)
                   ))
      ("NOTRACK")                       ;Disable connection tracking.

      ("RATEEST" .                  ;Gather statistics -> rate est. calc -> save
                 ("--rateest-name"  ;name
                  "--rateest-interval"  ;amount{s|ms|us}
                  "--rateest-ewmalog"   ;value
                  ))
      ("REDIRECT" .                     ;Redirect packet to self.
                  ("--to-ports"         ;port[-port]
                   "--random" ))
      ("REJECT" .          ;Send error packet in response to the matched packet.
                ("--reject-with" .      ;type
                                 ("icmp-net-unreachable"
                                  "icmp-host-unreachable"
                                  "icmp-port-unreachable"
                                  "icmp-proto-unreachable"
                                  "icmp-net-prohibited"
                                  "icmp-host-prohibited"
                                  "icmp-admin-prohibited")))
      ("SAME" .           ;Return mapped address range to client per connection.
              ("--to ipaddr"            ;[-ipaddr]
               "--nodst"
               "--random"))
      ("SECMARK" .                      ;Set security mark value of packet.
                 ("--selctx"            ;security_context
                  ))
      ("SET" .                ;Add/Remove entry from IP sets defined per `ipset'
             ("--add-set"     ;setname flag[,flag...]
              "--del-set"     ;setname flag[,flag...]
              ))
      ("SNAT" .                ;Specify modification of packet's source address.
              ("--to-source"   ;ipaddr[-ipaddr][:port[-port]]
               "--random"))
      ("TCPMSS" .                       ;Alter MSS value of TCP SYN packets.
                ("--set-mss"            ;value
                 "--clamp-mss-to-pmtu"))
      ("TCPOPTSTRIP" .                  ;strip TCP option off packet.
                     ("--strip-options" ;option[,option...]
                      ))
      ("TOS" .                        ;Set Type of Service field in IPv4 header.
             ("--set-tos" .           ;
                          ("--and-tos"  ;bits
                           "--or-tos"   ;bits
                           "--xor-tos"  ;bits
                           )))
      ("TPROXY" .                       ;redirect packet to local socket.
                ("--on-port"            ;port
                 "--on-ip"              ;address
                 "--tproxy-mark"        ;value[/mask]
                 ))
      ("TRACE"           ; Mark packets for kernel logging of rules traversed.
       ;; :EXAMPLE "TRACE:  tablename:chainname:type:rulenum "
       )
      ("TTL" .                          ;Modify IPv4 TTL  header field.
             ("--ttl-set"               ;value
              "--ttl-dec"               ;value
              "--ttl-inc"               ;value
              ))
      ("ULOG" .                         ;Userspace logging of matching packets.
              ("--ulog-nlgroup"         ;nlgroup
               "--ulog-prefix"          ;prefix
               "--ulog-cprange"         ;size
               "--ulog-qthreshold"      ;size
               )))))
"*Lisp alist of symbols, flags, etc. for GNU/Linux `iptables' i.e. `netfilter'.
List keys associate elements of list which are all strings.
List keys include:\n:IPTABLES-TARGETS\n:IPTABLES-TABLES\n:IPTABLES-COMMANDS
:IPTABLES-PARAMETERS\n:IPTABLES-OPTIONS\n:IPTABLES-MATCH-EXTENSIONS
:IPTABLES-TARGET-EXTENSIONS\n
For equivalent list with all elts as symbols :SEE `*mon-iptables-alist-as-sym*'.\n
List produced from :SOURCE iptables man page.
:SEE-ALSO `*regexp-clean-iptables*',
`mon-iptables-pp-key', `mon-iptables-pp-as-sym',
`mon-iptables-make-regexps', `mon-cln-iptables-short-form'\n►►►")
;;
;; :TEST-ME (mapcar 'car *mon-iptables-alst*)
;;
;;; (progn (makunbound '*mon-iptables-alst*) (unintern '*mon-iptables-alst*))

;;; ==============================
(provide 'mon-iptables-vars)
;;; ==============================

;;; ================================================================
;;; mon-iptables-vars.el ends here
;;; EOF
