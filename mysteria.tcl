package require Tcl
package require sha1
package require base64
package require pki
package require aes
package require udp
package require crc32
package require Tk
package require Trf
package require tcl::chan::events
package require tcl::chan::random
package require Memchan
package require snack
package require sound
package require Img
package require tdom

# globals

set ::myport {}
array set ::b {}
array set ::peerstore {}
array set ::valuestore {}
array set ::p {}
array set ::waitvalue {}
array set ::sources {}
array set ::contacts {}
array set ::headers {}
array set ::my_groups {}
array set ::groups {}
array set ::jgroups {}
array set ::group_to_sig {}
array set ::group_to_sigreq {}
array set ::person_to_sig {}
array set ::person_to_sigreq {}
array set ::file_by_hash {}
array set ::hash_by_file {}
array set ::dl_by_hash {}
array set ::dlstate_by_hash {}
array set ::buddies {}
array set ::statuses {}
array set ::tcp {}
array set ::tcp_fail {}
set ::tcp_fail(count) 0 
set ::audio_state 0
set ::ml_cur_group {}
set ::ml_cur_group_l {}
set ::ml_cur_person {}
set ::ml_cur_person_l {}
set ::ml_topline {}
set ::buddies_l {}
set ::buddies_k {}
set ::jgroups_l {}
set ::jgroups_m {}
set ::jgroups_i {}
set ::jgroups_k {}
array set ::keys {}
array set ::notices {}
set ::contactlist {}
set ::contactlist_k {}
set ::contactsearch {}
set ::grouplist {}
set ::grouplist_k {}
set ::groupsearch {}
set ::search {}
set ::searchfield {}
set ::text {}
set ::waitlast 0
set ::dllast 0
set ::chqlast 0
set ::gchqlast 0
set ::peerlast 0
set ::tick [clock seconds] 
set ::filepath {}
set ::msglist_t {}
set ::msglist_k {}
set ::default_chunksize 65536
set ::mekey {}
set ::me {}
set ::menick {}
set ::mycontact {}
set ::mode {}
set ::c_bday {}
set ::c_sex {}
set ::c_country {}
set ::c_city {}
set ::font {Sans 9}
set ::listfont {Monospace 9}
set ::randomseed [clock microseconds]
set ::randomchan {}
set ::plain_allowed 0
set ::group_host_mode 1
set ::group_sync_allowed 0
set ::line_th 1

# run

proc main args {
	wm withdraw .
	store_select
}

proc launch {} {
	puts "Starting"

	set ::randomchan [::tcl::chan::random $::randomseed]
	fconfigure $::randomchan -translation binary

	file mkdir $::filepath
	file mkdir [file join $::filepath "downloads"]
	file mkdir [file join $::filepath "temp"]
	file mkdir [file join $::filepath "mailnews"]
	file mkdir [file join $::filepath "mailnews" "dec"]
	file mkdir [file join $::filepath "mailnews" "plain"]
	file mkdir [file join $::filepath "mailnews" "archive"]
	file mkdir [file join $::filepath "chat"]
	file mkdir [file join $::filepath "gchat"]
	file mkdir [file join $::filepath "mlhdr"]
	file mkdir [file join $::filepath "mlht"]
	file mkdir [file join $::filepath "mlphdr"]
	file mkdir [file join $::filepath "mlsrc"]
	file mkdir [file join $::filepath "mltemp"]

	load_id
	load_port
	load_contact
	load_peers
	load_buckets
	load_values
	load_contacts
	load_headers
	load_groups
	load_jgroups
	load_buddies
	load_sources
	load_files
	load_dls
	load_keys
	load_sig
	puts "listen port $::myport"
	#set ::c [listen]
	set ::ct [tcp_listen]

	sanitize_stores

	set oneyear [expr "86400*365"]
	archive_mlhdr $oneyear
	archive_mlphdr $oneyear
	archive_mailnews $oneyear

	check_waitvalues
	#check_downloads
	check_chatqueue
	check_gchatqueue
	check_peers
	
	#show_group_selection

	every 1000 {
		#puts "check schedule"
		# maintenance of a dozen queues
		#check_waitvalues
		#check_downloads
		#check_chatqueue
		#check_peers
		# UI updates
		#update_widgets
		#update_mail {}
		#update_directory
		#update_buddies
		set ::tick [clock seconds]
		if { !([winfo exists .m] || [winfo exists .b] || [winfo exists .g] || [winfo exists .startshield] || [winfo exists .sss]) } {
			write_all ; exit
		}
	}
	
	every 15000 {
		sanitize_stores
	}
	
	every 180000 {
		after idle [list send_my_status "O" "online"]
	}

	every 180000 {
		# dl
		#after idle check_downloads
		# set sources
		after idle sc_setsources
		#	hash files in share directory
		after idle hash_files
		# publish contact
		#after 15000 [list sc_publishcontact $::mycontact]
		# save everything to files once in 15sec,
		# to be certain this works without changing
		# identifiers all the time
		after idle write_all
	}

	#every 900000 {
	#	after idle sol_store
	#}

	make_nmenu

	show

	vwait forever
	close $::ct
} 

proc store_select {} {
	if { [winfo exists .sss] == 1 } {
		return
	}
	set ::filepath [file join "." ".store"]
	set dirs [glob -nocomplain -type d -directory $::filepath *]
	set choice {}
	foreach dir $dirs {
		lappend choice [lindex [file split $dir] end]
	}
	if { $choice == "" } {
		lappend choice "default"
	} 
	toplevel .sss
	wm title .sss "Select store dir"
	pack [panedwindow .sss.p -ori vert ]
	.sss.p add [frame ".sss.l"] -stretch never
	pack [label .sss.l.l -text "Select store dir:" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left 
	.sss.p add [frame ".sss.c"] -stretch never
	pack [entry .sss.c.e -textvar ::sel_store -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left 
	set storemenu [tk_optionMenu .sss.c.c ::sel_store {*}$choice]
	$storemenu configure -font $::font -activebackground {#606060} -activeforeground {#000000}
	.sss.c.c configure -font $::font -activebackground {#606060} -activeforeground {#000000} -highlightcolor {#909090} -highlightbackground {#606060}
	pack .sss.c.c -fill both -side left 
	.sss.p add [frame ".sss.e"] -stretch never
	pack [button .sss.e.c -text "select" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {set ::filepath [file join $::filepath $::sel_store] ; after 1000 [list destroy .sss] ; launch}] -fill both -side right
}

proc write_all {} {
		puts "write all"
		write_id
		write_port
		write_contact
		write_peers
		write_buckets
		write_values
		write_contacts
		write_headers
		write_groups
		write_jgroups
		write_buddies
		write_sources
		write_files
		write_dls
		#write_keys
		write_sig
}

proc every {ms body} {
	eval $body; after $ms [info level 0]
}

proc head_all {} {
	foreach {gid grp} [array get ::jgroups] {
		ml_grouphead $grp
	}
	foreach {hash contact} [array get ::buddies] {
		ml_personhead $contact
	}
}

proc show {} {
	ttk::style theme use clam
	wm withdraw .
	show_mail
	#show_buddylist
	#show_startshield
}

proc archive_mlhdr {a} {
	set thr [expr "[clock sec]-$a"]
	set g [array names ::jgroups]
	foreach grp $g {
		set hdrs [lrange [ml_get_hdrs $grp] 2 end]
		set nhdrs {}
		set ohdrs {}
		#puts "hdrs $hdrs"
		foreach hdr $hdrs {
			set h [header_to_dict $hdr]
			if { $h == "" } {
				puts "bad header $hdr"
				continue
			}
			set epoch [dict get $h epoch]
			puts "epoch $epoch vs thr $thr"
			if { $epoch > $thr } {
				lappend nhdrs $hdr
			} else {
				#set hash [dict get $h hash]
				#file rename [file join $::filepath "mailnews" "$hash" ] [file join $::filepath "mailnews" "archive" $hash ]
				lappend ohdrs $hdr
			}
		}
		file delete [file join $::filepath "mlhdr" $grp]
		ml_add_hdrs $grp $nhdrs
		ml_add_hdrs "archive_$grp" $ohdrs
	}
}

proc archive_mlphdr {a} {
	set thr [expr "[clock sec]-$a"]
	set p [array names ::peerstore]
	foreach per $p {
		set hdrs [lrange [ml_get_personhdrs $per] 2 end]
		set nhdrs {}
		set ohdrs {}
		puts "hdrs $hdrs"
		foreach hdr $hdrs {
			set h [header_to_dict $hdr]
			if { $h == "" } {
				puts "bad header $hdr"
				continue
			}
			set epoch [dict get $h epoch]
			puts "epoch $epoch vs thr $thr"
			if { $epoch > $thr } {
				lappend nhdrs $hdr
			} else {
				#set hash [dict get $h hash]
				#file rename [file join $::filepath "mailnews" "$hash" ] [file join $::filepath "mailnews" "archive" $hash ]
				lappend ohdrs $hdr
			}
		}
		file delete [file join $::filepath "mlphdr" $per]
		ml_add_personhdrs $per $nhdrs
		ml_add_personhdrs "archive_$per" $ohdrs
	}
}

proc archive_mailnews {a} {
	set thr [expr "[clock sec]-$a"]
	set files [glob -nocomplain -type f -directory [file join $::filepath "mailnews"] *]
	foreach f $files {
		puts "file to check $f"
		file stat $f statvar
		set epoch $statvar(ctime)
		if { $epoch > $thr } {
			puts "new enough"
		} else {
			puts "old, moving to archive"
			set af [file join $::filepath "mailnews" "archive" [lindex [file split $f] end]]
			if { [file exists $af] == 0 } {
				file rename $f $af
			}
		}
	}
}

proc sanitize_stores {} {
	foreach {key val} [array get ::sources] {
		array unset ::sources "$key"
		if { [regexp -all {:} $val] > 0 || [string length $val] != 40} {	
			continue
		} else {	
			array set ::sources [list "$key" "$val"]
		}
	}
	foreach {key val} [array get ::headers] {
		array unset ::headers "$key"
		if { [header_to_dict $val] == "" || [regexp -all {:} $val] < 11} {
			continue
		} else {
			array set ::headers [list "$key" "$val"]
		}
	}
	foreach {key val} [array get ::groups] {
		array unset ::groups "$key"
		if { [ml_groupdict $val] == "" } {
			continue
		} else {
			array set ::groups [list "$key" "$val"]
		}
	}
	foreach {key val} [array get ::contacts] {
		array unset ::contacts "$key"
		if { [contact_to_dict $val] == "" } {
			continue
		} else {
			array set ::contacts [list "$key" "$val"]
		}
	}
}

proc hash_files {} {
	puts "hash files"
	set dir [file join $::filepath "share" ]
	if { [ file exists $dir ] == 0 } {
		file mkdir $dir
	}
	if { [ file isdirectory $dir ] == 0 } {
		puts "ERR $dir is not a directory"
		return
	}
	foreach file [glob -nocomplain -directory $dir *] {
		if { [file isfile $file] == 1 } { 
			hash_file $file
		}
	}
}

proc hash_file {file} {
	puts "hashing $file"
	set d {}
	set file_e [wrap $file]
	dict set d name $file_e
	dict set d size [file size $file]
	if { [array names ::hash_by_file $file_e] != "" } {
		puts "file $file already hashed"
		return
	}
	set hash [ ::sha1::sha1 -hex -file $file ]
	array set ::file_by_hash [list $hash $d]
	array set ::hash_by_file [list $file_e $hash]
	array set ::sources [list $hash $::me]
	array set ::sources [list [::sha1::sha1 $file] $::me]
	array set ::sources [list [::sha1::sha1 $hash] $::me]
}

proc show_startshield {} {
	wm title . "Mysteria"
	set w ".shield"
	pack [panedwindow $w -ori vert] -fill both
	$w add [frame "$w.f"] -stretch never
	pack [label "$w.f.l" -text "Mysteria" -font {Serif 18} ] -fill both
	pack [label "$w.f.ll" -text "A \"worse is better\"\ncommunication\nprogram\n(TBH worse is all I can,\nthus it's better)" -font {Serif 9} ] -fill both
	$w add [frame "$w.b"] -stretch never
	#pack [button "$w.b.gdir" -text {GDirectory} -command {show_group_directory} -font {Serif 9} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} ] -fill both 
	#pack [button "$w.b.cdir" -text {Directory} -command {show_directory} -font {Serif 9} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} ] -fill both 
	pack [button "$w.b.mn" -text {Mail/News} -command {show_mail} -font {Serif 9} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} ] -fill both 
	pack [button "$w.b.gl" -text {Groups} -command {show_grouplist} -font {Serif 9} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} ] -fill both 
	pack [button "$w.b.bl" -text {Buddies} -command {show_buddylist} -font {Serif 9} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} ] -fill both 
	pack [button "$w.b.gsig" -text {GRequests} -command {show_mygroups} -font {Serif 9} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} ] -fill both 
	pack [button "$w.b.psig" -text {Requests} -command {show_sigmanager p *} -font {Serif 9} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} ] -fill both 
	pack [button "$w.b.dls" -text {DLState} -command {show_dlstate} -font {Serif 9} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} ] -fill both 
	pack [button "$w.b.dbg" -text {Debug} -command {show_debug} -font {Serif 9} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} ] -fill both 
	pack [button "$w.b.kill" -text {Exit} -command {write_all;exit} -font {Serif 9} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} ] -fill both 
	wm deiconify .
}

proc show_buddy_del {hash} {
	puts "buddy del $hash request"
	if { [winfo exists .bd] == 1} {
		return
	}
	if { [llength $hash] > 1 } {
		return
	}
	set buddy [lindex [array get ::buddies $hash] 1]
	set b [contact_to_dict $buddy]
	toplevel .bd
	wm title .bd "Delete buddy"
	pack [panedwindow .bd.p -ori vert] -fill both -expand 1
	.bd.p add [frame .bd.p.f0]
	pack [label .bd.p.f0.q -text "Do you want to delete buddy\n[dict get $b nickname] with id\n<[dict get $b peerid]>,\nchat #$hash?" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	.bd.p add [frame .bd.p.f1]
	pack [button .bd.p.f1.yes -text "delete" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "array unset ::buddies $hash ; after 50 update_buddies ; destroy .bd ; destroy .bv"] -fill both -side right
	pack [button .bd.p.f1.no -text "cancel" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "destroy .bd"] -fill both -side right 
	
}

proc show_buddy_details {hash} {
	puts "chat_show_buddy_details $hash"
	if { [winfo exists .bv] == 1} {
		return
	}
	if { [llength $hash] != 1 } {
		return
	}
	set buddy [lindex [array get ::buddies $hash] 1]
	set b [contact_to_dict $buddy]

	append t "Nickname:\t [dict get $b nickname]\n"
	append t "Id:\t [dict get $b peerid]\n"
	append t "\n%%%\n\n"
	append t "Birthday:\t [dict get $b birthday]\n"
	append t "Sex:\t [dict get $b sex]\n"
	append t "Country:\t [dict get $b country]\n"
	append t "City:\t [dict get $b city]\n"
	append t "\n%%%\n\n"
	append t "Pubkey:\n[dict get $b pubkey]\n\n"
	append t "\n%%%\n\n"

	toplevel .bv
	wm title .bv "View buddy"
	pack [panedwindow .bv.p -ori vert] -fill both -expand 1
	.bv.p add [frame .bv.p.f0]
	pack [label .bv.p.f0.t -text $t -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont -justify left ] -fill both -side left
	.bv.p add [frame .bv.p.f1]
	pack [button .bv.p.f1.c -text "chat" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "show_chatwindow $hash"] -fill both -side left
	#pack [button .bv.p.f1.m -text "mail" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "set ::mode p ; show_editor $::buddies($hash)"] -fill both -side left
	#pack [button .bv.p.f1.a -text "call" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "show_audio_caller $hash"] -fill both -side left
	#pack [button .bv.p.f1.v -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "destroy .bv"] -fill both -side right
	pack [button .bv.p.f1.x -text "del" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "show_buddy_del $hash"] -fill both -side right 
}

proc chat_showoffer {hash body} {
	puts "chat_showoffer $hash $body"
	array set ::buddies [list $hash $body]
	after 50 update_buddies
	if { [winfo exists .co] == 1} {
		return
	}
	set b [contact_to_dict $body]
	toplevel .co
	wm title .co "Chat offer from [dict get $b nickname]"
	pack [panedwindow .co.p -ori vert] -fill both -expand 1
	.co.p add [frame .co.b0]
	pack [label .co.b0.ml -text "id: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left 
	pack [label .co.b0.m -text [dict get $b peerid] -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 24 ] -fill both -side left 
	pack [button .co.b0.v -text "ok" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "after 50 update_buddies ; destroy .co"] -fill both -side right
	pack [button .co.b0.x -text "delete" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "array unset ::buddies $hash ; after 50 update_buddies ; destroy .co"] -fill both -side right 
	.co.p add [frame .co.b1]
	pack [label .co.b1.tl -text "Nickname: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font] -fill both -side left 
	pack [label .co.b1.t -text [dict get $b nickname] -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 24 ] -fill both -side right
	.co.p add [frame .co.b2]
	pack [label .co.b2.lage -text "Birthday: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .co.b2.age -text [dict get $b birthday] -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 24 ] -fill both -side right 
	.co.p add [frame .co.b3]
	pack [label .co.b3.lsex -text "Sex: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .co.b3.sex -text [dict get $b sex] -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 24 ] -fill both -side right
	.co.p add [frame .co.b4]
	pack [label .co.b4.lcountry -text "Country: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .co.b4.country -text [dict get $b country] -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 24 ] -fill both -side right
	.co.p add [frame .co.b5]
	pack [label .co.b5.lcity -text "City: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .co.b5.city -text [dict get $b city] -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 24 ] -fill both -side right
}

proc show_contactform {} {
	if { [winfo exists .cf] == 1} {
		return
	}
	toplevel .cf
	wm title .cf {Create contact}
	pack [panedwindow .cf.p -ori vert] -fill both -expand 1
	.cf.p add [frame .cf.b0]
	pack [label .cf.b0.ml -text "id: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left 
	pack [label .cf.b0.m -text "$::me" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 24 ] -fill both -side left 
	pack [button .cf.b0.v -text "commit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {sc_publishcontact [form_contact] ; destroy .cf}] -fill both -side right
	#pack [button .cf.b0.x -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {destroy .cf}] -fill both -side right 
	.cf.p add [frame .cf.b1]
	pack [label .cf.b1.tl -text "Nickname: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font] -fill both -side left 
	pack [entry .cf.b1.t -textvariable ::menick -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -width 24 ] -fill both -side right
	.cf.p add [frame .cf.b2]
	pack [label .cf.b2.lage -text "Birthday: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [entry .cf.b2.age -textvariable ::c_bday -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -width 24 ] -fill both -side right 
	.cf.p add [frame .cf.b3]
	pack [label .cf.b3.lsex -text "Sex: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [entry .cf.b3.sex -textvariable ::c_sex -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -width 24 ] -fill both -side right
	.cf.p add [frame .cf.b4]
	pack [label .cf.b4.lcountry -text "Country: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [entry .cf.b4.country -textvariable ::c_country -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -width 24 ] -fill both -side right
	.cf.p add [frame .cf.b5]
	pack [label .cf.b5.lcity -text "City: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [entry .cf.b5.city -textvariable ::c_city -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -width 24 ] -fill both -side right
}

proc show_directory {} {
	if { [winfo exists .d] == 1} {
		return
	}
	toplevel .d
	wm title .d {Directory}
	pack [panedwindow .d.p -ori vert ] -fill both -expand 1
	.d.p add [frame ".d.e"]
	pack [button .d.e.c -text "create" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_contactform}] -fill both -side left 
	pack [button .d.e.p -text "publish" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {sc_publishcontact $::mycontact}] -fill both -side left 
	pack [entry .d.e.e -textvariable ::contactfield -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont ] -fill both -side left
	pack [button .d.e.s -text "search" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {
		.d.e.s configure -state disabled;
		.d.e.st configure -state normal;
		show_contacts [sc_get_contacts [prep_contact_keys $::contactfield]]
	}] -fill both -side right
	pack [button .d.e.st -text "stop" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {
		.d.e.s configure -state normal; 
		.d.e.st configure -state disabled;
		sc_stop $::contactsearch
	} -state disabled] -fill both -side right
	#pack [button .d.e.o -text "mail" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {set ::mode {p} ; set ::ml_cur_person [lindex $::contactlist_k [lindex [.d.l.l index active] 0]] ; show_editor {}}] -fill both -side right
	#pack [button .d.e.det -text "detail" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_buddy_details [lindex $::contactlist_k [lindex [.d.l.l index active] 0]]}] -fill both -side right
	pack [button .d.e.cht -text "add" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {chat_offer [chat_add [lindex $::contactlist_k [lindex [.d.l.l index active] 0]]] ; destroy .d}] -fill both -side right
	#pack [button .d.e.x -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {destroy .d}] -fill both -side right
	.d.p add [frame ".d.l"]
	pack [scrollbar .d.l.y -activebackground {#606060}  -troughcolor {#606060}  -command ".d.l.l yview"] -fill y -side right
	pack [listbox .d.l.l -listvariable ::contactlist -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont -height 12 -yscrollc ".d.l.y set"] -fill both -expand 1 -side right
}

proc show_group_details {group} {
	if { [winfo exists .sgf] == 1} {
		return
	}
	if { $group == "" } {
		return
	}
	set g [ml_groupdict $group]
	toplevel .sgf
	wm title .sgf {View group}
	pack [panedwindow .sgf.p -ori vert] -fill both -expand 1
	.sgf.p add [frame .sgf.b1]
	if { [array get ::jgroups [dict get $g gid]] == "" } {
		pack [button .sgf.b1.a -text "add" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "catch { set ::groups([dict get $g gid]) $group ; set ::jgroups([dict get $g gid]) $group ; ml_add_srcs [dict get $g gid] $::me ;destroy .sgf ; after idle [list sc_publishcontact $::mycontact] ; after idle [list ml_grouphead $group] }"] -fill both -side right
	} else {
		pack [button .sgf.b1.c -text "chat" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "show_gchatwindow [dict get $g gid]"] -fill both -side left
		pack [button .sgf.b1.m -text "mail" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "show_gmailwindow [dict get $g gid]"] -fill both -side left
		pack [button .sgf.b1.pub -text "publish" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "sc_publishgroup $group"] -fill both -side left
		pack [button .sgf.b1.d -text "del" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "array unset ::jgroups [dict get $g gid] ; destroy .sgf ; after idle [list ml_grouphead $group]"] -fill both -side right
	}
	#pack [button .sgf.b1.x -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {destroy .sgf}] -fill both -side right 
	.sgf.p add [frame .sgf.b2]
	pack [label .sgf.b2.lname -text "Name: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font] -fill both -side left 
	pack [label .sgf.b2.name -text "[dict get $g name]" -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont] -fill both -side left 
	.sgf.p add [frame .sgf.b3]
	pack [label .sgf.b3.ldesc -text "Desc: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .sgf.b3.desc -text "[dict get $g desc]" -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont ] -fill both -side left
	.sgf.p add [frame .sgf.b4]
	pack [label .sgf.b4.ldesc -text "Author peerID: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .sgf.b4.desc -text "[dict get $g peerid]" -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont ] -fill both -side left
	.sgf.p add [frame .sgf.b5]
	pack [label .sgf.b5.ldesc -text "Author psig: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .sgf.b5.desc -text "[string range [wrap [dict get $g psig]] 0 63]..." -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont ] -fill both -side left
	.sgf.p add [frame .sgf.b6]
	pack [label .sgf.b6.lpkey -text "Public key: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	.sgf.p add [frame .sgf.b7]
	pack [label .sgf.b7.pkey -text "[dict get $g pkey]" -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont ] -fill both -side left
	.sgf.p add [frame .sgf.b8]
	pack [label .sgf.b8.lgid -text "GID: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .sgf.b8.gid -text "[dict get $g gid]" -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont ] -fill both -side left
}

proc show_groupform {} {
	if { [winfo exists .gf] == 1} {
		return
	}
	set commit_cmd {
		array set ::groups [list $::g_gid $::g_group]
		array set ::jgroups [list $::g_gid $::g_group]
		array set ::my_groups [list $::g_gid [wrap $::g_key]]
		sc_publishgroup $::g_group	
		ml_add_srcs $::g_gid $::me
		ml_add_sigreq g $::g_gid $::mycontact
		set pkey [::pki::pkcs::parse_key $::g_key]
		set req [lindex [array get ::group_to_sigreq "$::g_gid,$::me"] end]
		set sig "$req:[wrap [::pki::sign $req $pkey]]"
		array set ::group_to_sig [list "$::g_gid,$::me" $sig]
		destroy .gf
		ml_grouphead $::g_group
	}
	toplevel .gf
	wm title .gf {Create group}
	pack [panedwindow .gf.p -ori vert] -fill both -expand 1
	.gf.p add [frame .gf.b0]
	pack [label .gf.b0.ml -text "Enter group name and description: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left 
	pack [button .gf.b0.v -text "commit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command $commit_cmd] -fill both -side right
	pack [button .gf.b0.g -text "generate" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {generate_group}] -fill both -side right
	#pack [button .gf.b0.x -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {destroy .gf}] -fill both -side right 
	.gf.p add [frame .gf.b1]
	pack [label .gf.b1.lname -text "Name: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font] -fill both -side left 
	pack [entry .gf.b1.name -textvariable ::g_name -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont -width 24 ] -fill both -side right
	.gf.p add [frame .gf.b2]
	pack [label .gf.b2.ldesc -text "Desc: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [entry .gf.b2.desc -textvariable ::g_desc -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont -width 24 ] -fill both -side right 
	.gf.p add [frame .gf.b3]
	pack [label .gf.b3.ldesc -text "peerID: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .gf.b3.desc -textvariable ::me -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont -width 24 ] -fill both -side right 
	.gf.p add [frame .gf.b5]
	pack [label .gf.b5.lkey -text "Key: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	.gf.p add [frame .gf.b6]
	pack [text .gf.b6.key -wrap word -yscrollc {.gf.b6.key_sb set} -height 12 -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont ] -fill both -expand 1 -side left
	pack [scrollbar .gf.b6.key_sb -activebackground {#606060}  -troughcolor {#606060}  -command {.gf.b6.key yview}] -fill y -side right
	.gf.p add [frame .gf.b7]
	pack [label .gf.b7.lpkey -text "Public key: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	.gf.p add [frame .gf.b8]
	pack [label .gf.b8.pkey -textvariable ::g_pkey -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont ] -fill both -side left
	.gf.p add [frame .gf.b9]
	pack [label .gf.b9.lgid -text "GID: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .gf.b9.gid -textvariable ::g_gid -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont ] -fill both -side left
	proc generate_group {} {
		set key [.gf.b6.key get 1.0 end]
		set gkey {}
		catch {
		set gkey [::pki::pkcs::parse_key $key]
		}
		if { $gkey == "" } {
			set gkey [::pki::rsa::generate 1024]
			set key [::pki::key $gkey]
		}
		set gpkey [::pki::public_key $gkey]
		set gid [::sha1::sha1 -hex $gpkey]
		set psig [::pki::sign $::me $gkey]
		set ::g_key $key
		set ::g_pkey $gpkey
		set ::g_gid $gid
		.gf.b6.key delete 1.0 end
		.gf.b6.key insert end $key
		set g {}
		dict set g gid $::g_gid
		dict set g name $::g_name
		dict set g desc $::g_desc
		dict set g pkey $::g_pkey
		dict set g epoch [clock seconds] 
		dict set g peerid $::me
		dict set g psig $psig
		set ::g_group [ml_dictgroup $g]
	}
}

proc show_group_directory {} {
	if { [winfo exists .gd] == 1} {
		return
	}
	toplevel .gd
	wm title .gd {Group directory}
	pack [panedwindow .gd.p -ori vert ] -fill both -expand 1
	.gd.p add [frame ".gd.e"]
	pack [button .gd.e.c -text "create" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_groupform}] -fill both -side left 
	pack [entry .gd.e.e -textvariable ::groupfield -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont ] -fill both -side left
	pack [button .gd.e.s -text "search" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {
		.gd.e.s configure -state disabled;
		.gd.e.st configure -state normal;
		show_groups [sc_get_groups [prep_group_keys $::groupfield]]
	}] -fill both -side right
	pack [button .gd.e.st -text "stop" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {
		.gd.e.s configure -state normal; 
		.gd.e.st configure -state disabled;
		sc_stop $::groupsearch
	} -state disabled] -fill both -side right
	pack [button .gd.e.o -text "detail" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_group_details [lindex $::grouplist_k [lindex [.gd.l.l index active] 0]] ; destroy .gd}] -fill both -side right
	#pack [button .gd.e.x -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {destroy .gd}] -fill both -side right
	.gd.p add [frame ".gd.l"]
	pack [scrollbar .gd.l.y -activebackground {#606060}  -troughcolor {#606060}  -command ".gd.l.l yview"] -fill y -side right
	pack [listbox .gd.l.l -listvariable ::grouplist -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont -height 12 -yscrollc ".gd.l.y set"] -fill both -expand 1 -side right
}

proc make_menu {w} {
	set m "$w.menu"
	set mc "$w.menu.menu"
	if { [winfo exists $m] == 1 } {
		return
	}
	if { [winfo exists $w] == 0 } {
		return
	}
	menu $m -type menubar -activeforeground {#000000} -activebackground {#606060} -font $::font
	menu $mc -tearoff 0 -activeforeground {#000000} -activebackground {#606060} -font $::font
	$m add cascade -menu $mc -label {menu}
	$mc add command -command {show_group_directory} -label {group directory} 
	$mc add command -command {show_directory} -label {directory} 
	$mc add command -command {show_mail} -label {news/mail} 
	$mc add command -command {show_grouplist} -label {groups} 
	$mc add command -command {show_buddylist} -label {buddies} 
	$mc add command -command {show_mygroups} -label {my groups} 
	$mc add command -command {show_sigmanager p *} -label {my requests} 
	$mc add command -command {show_dlstate} -label {dlstate} 
	#$mc add command -command {show_audio} -label {audio} 
	#$mc add command -command {show_files {}} -label {files} 
	$mc add command -command {show_debug} -label {debug} 
	#$mc add command -command "destroy $w" -label {exit} 
	$mc add command -command {write_all;exit} -label {kill} 
	$w configure -menu $m
}

proc make_nmenu {} {
	set m .nmenu
	if { [winfo exists $m] == 1 } {
		return
	}
	menu $m -tearoff 0 -activeforeground {#000000} -activebackground {#606060} -font $::font
	$m add command -command {show_group_directory} -label {group directory} 
	$m add command -command {show_directory} -label {directory} 
	$m add command -command {show_mail} -label {news/mail} 
	$m add command -command {show_grouplist} -label {groups} 
	$m add command -command {show_buddylist} -label {buddies} 
	$m add command -command {show_mygroups} -label {my groups} 
	$m add command -command {show_sigmanager p *} -label {my requests} 
	#$m add command -command {show_downloads} -label {downloads} 
	$m add command -command {show_dlstate} -label {dlstate} 
	#$m add command -command {show_audio} -label {audio} 
	$m add command -command {show_debug} -label {debug} 
	$m add command -command {write_all;exit} -label {kill} 
}

proc ml_screen {mode id host port} {
	#puts "ml_screen host $host port $port"
	set found {}
	foreach peer [lsearch -all -inline [array get ::peerstore] "*:$host:$port"] {
		lappend found [lindex [split $peer {:}] 0]
	}
	#puts "ml_screen found all $found"
	set found [lindex [lsort -unique $found] 0]
	#puts "ml_screen found unique $found"
	set ret [ml_check_sig $mode $id $found 0]
	#puts "ml_screen ret $ret"
	if { $ret != 0 } {
		set cfound [lsearch -all -inline [array get ::contacts] "$found:*"]
		#puts "ml_screen cfound $cfound"
		foreach contact $cfound {
			#puts "ml_screen contact $contact"
			#puts "ml_screen add sigreq mode $mode id $id contact $contact"
			ml_add_sigreq $mode $id $contact
		}
	}
	return $ret
}

proc ml_check_sig {mode id peerid check} {
	#puts "ml_check_sig mode $mode id $id peerid $peerid"
	if { $mode == {g} } {
		set fsig [array get ::group_to_sig "$id,$peerid"]
		if { $fsig == "" } {
			puts "no such sig $id,$peerid"
			#sc_get_contacts [prep_contact_keys $peerid]
			return -1
		}
		if { $check != 1 } {
			return 0
		}
		set grp [array get ::groups "$id"]
		set g [ml_groupdict $grp]
		if { $g == "" } {
			puts "no such group"
			return -1
		}
		set pkey [dict get $g pkey]
		set tock [lrange [split $fsig {:}] 0 end-1]
		set sig [unwrap [lindex [split $fsig {:}] end]]
		set ver [::pki::verify $sig $tock [::pki::pkcs::parse_public_key $pkey]]
		if { $ver == "false" } {
			puts "sigerr"
			return -1
		} else {
			return 0
		}
	} elseif { $mode == {p} } {
		set fsig [array get ::person_to_sig "$::me,$peerid"]
		if { $fsig == "" } {
			puts "no such sig $::me,$peerid"
			#sc_get_contacts [prep_contact_keys $peerid]
			return -1
		}
		if { $check != 1 } {
			return 0
		}
		set pkey [::pki::public_key $::mekey]
		set tock [lrange [split $fsig {:}] 0 end-1]
		set sig [unwrap [lindex [split $fsig {:}] end]]
		set ver [::pki::verify $sig $tock [::pki::pkcs::parse_public_key $pkey]]
		if { $ver == "false" } {
			puts "sigerr"
			return -1
		} else {
			return 0
		}
	}
	return -1
} 

proc ml_add_sigreq {mode id contact} {
	if { $mode == {g} } {
		set c [contact_to_dict $contact]
		if { $c == "" } {
			return
		}
		set peerid [dict get $c peerid]
		array set ::group_to_sigreq [list "$id,$peerid" $contact]
	} elseif { $mode == {p} } {
		set c [contact_to_dict $contact]
		if { $c == "" } {
			return
		}
		set peerid [dict get $c peerid]
		array set ::person_to_sigreq [list "$::me,$peerid" $contact]
	}
}

proc show_mygroups {} {
	if { [winfo exists .mg] == 1 } {
		return
	}
	toplevel .mg
	wm title .mg "My groups"
	pack [panedwindow .mg.p -ori vert -width 240] -fill both -expand 1
	.mg.p add [frame ".mg.top"] -stretch never
	pack [label .mg.top.l -text "Pick a group:" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	.mg.p add [frame ".mg.l"] -stretch always 
	pack [scrollbar .mg.l.y -activebackground {#606060}  -troughcolor {#606060}  -command "tl_yview 1 .mg.l.l"] -fill y -side right
	pack [listbox .mg.l.l -listvariable ::my_groups_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 12 -yscrollc ".mg.l.y set"] -fill both -expand 1 -side right
	.mg.p add [frame ".mg.b"] -stretch never
	pack [button .mg.b.sm -text "Members" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_sigmanager g [lindex $::my_groups_i [lindex [.mg.l.l index active] 0]]} ] -fill both -side right
	set ::my_groups_l {}
	set ::my_groups_i {}
	set ::my_groups_k {}
	foreach {key val} [array get ::my_groups] {
		set d [ml_groupdict [lindex [array get ::groups "$key"] end]]
		if { $d == "" } {
			continue
		}
		set dis "[dict get $d name] <[dict get $d gid]>"
		lappend ::my_groups_l $dis
		lappend ::my_groups_i $key
		lappend ::my_groups_k $val
	}
}

proc show_sigmanager {mode id} {
	if { [winfo exists .sm] == 1 } {
		return
	}
	if { $mode == {} } {
		return
	}
	if { $id == {} } {
		return
	}
	# sigreq is a contact, but we sign and check peerid:epoch
	# sig is peerid:epoch:signature
	proc ref_cmd {mode id} {
	if { $mode == {g} } {
		set grps [array get ::my_groups "$id"]
		if { [llength $grps] == 0 } {	
			puts "Don't have key for group $id"
			return
		}
		set ::sm_req_i {}
		set ::sm_req_c {}
		set ::sm_req_s {}
		set ::sm_req_l {}
		set ::sm_add_i {}
		set ::sm_add_c {}
		set ::sm_add_s {}
		set ::sm_add_l {}
		foreach {key value} [array get ::group_to_sigreq "$id,*"] {	
			set c [contact_to_dict $value]
			if { $c == "" } {
				continue	
			}
			set nick [dict get $c nickname]
			set peerid [dict get $c peerid]
			set epoch [clock seconds]
			if { [array get ::group_to_sig $key] == "" } {
				lappend ::sm_req_i "$key"
				lappend ::sm_req_c "$value"
				lappend ::sm_req_s "$peerid:$epoch"
				lappend ::sm_req_l "R $nick <$peerid>"
			} else {
				sc_publishcontact $value
				lappend ::sm_add_i "$key"
				lappend ::sm_add_c "$value"
				lappend ::sm_add_s "$peerid:$epoch"
				lappend ::sm_add_l "S $nick <$peerid>"
			}
		}
	} elseif { $mode == {p} } {
		set ::sm_req_i {}
		set ::sm_req_c {}
		set ::sm_req_s {}
		set ::sm_req_l {}
		set ::sm_add_i {}
		set ::sm_add_c {}
		set ::sm_add_s {}
		set ::sm_add_l {}
		foreach {key value} [array get ::person_to_sigreq "$::me,*"] {
			set c [contact_to_dict $value]
			if { $c == "" } {
				continue	
			}
			set nick [dict get $c nickname]
			set peerid [dict get $c peerid]
			set epoch [clock seconds]
			if { [array get ::person_to_sig $key] == "" } {
				lappend ::sm_req_i "$key"
				lappend ::sm_req_c "$value"
				lappend ::sm_req_s "$peerid:$epoch"
				lappend ::sm_req_l "R $nick <$peerid>"
			} else {
				sc_publishcontact $value
				lappend ::sm_add_i "$key"
				lappend ::sm_add_c "$value"
				lappend ::sm_add_s "$peerid:$epoch"
				lappend ::sm_add_l "S $nick <$peerid>"
			}
		}
	}
	}

	proc add_cmd {mode id} {
		set i [lindex [.sm.req.l index active] 0]
		if { $mode == {g} } {
			set fid [lindex $::sm_req_i $i]
			if { [array get ::group_to_sig $fid] != "" } {
				puts "group_to_sig $fid already added"
				return
			}
			set pkey [::pki::pkcs::parse_key [unwrap $::my_groups($id)]]
			puts "pkey $pkey s [lindex $::sm_req_s $i]"
			set sig "[lindex $::sm_req_s $i]:[wrap [::pki::sign [lindex $::sm_req_s $i] $pkey]]"
			array set ::group_to_sig [list $fid $sig]
		} elseif { $mode == {p} } {	
			set fid [lindex $::sm_req_i $i]
			if { [array get ::person_to_sig $fid] != "" } {
				puts "person_to_sig $fid already added"
				return
			}
			set pkey $::mekey
			set sig "[lindex $::sm_req_s $i]:[wrap [::pki::sign [lindex $::sm_req_s $i] $pkey]]"
			array set ::person_to_sig [list $fid $sig]
		}
		ref_cmd $mode $id
		sc_publishcontact $::mycontact
		if { $mode == {g} } {
			set ::mode $mode
			ml_grouphead [lindex [array get ::jgroups $id] end]
		} elseif { $mode == {p} } {
			set ::mode $mode
			ml_personhead [lsearch -inline [array get ::buddies] "$id:*"]
		}
	}
	
	proc rem_cmd {mode id} {
		set i [lindex [.sm.add.l index active] 0]
		if { $mode == {g} } {
			set fid [lindex $::sm_add_i $i]
			array unset ::group_to_sig $fid
		} elseif { $mode == {p} } {
			set fid [lindex $::sm_add_i $i]
			array unset ::person_to_sig $fid
		}
		ref_cmd $mode $id
		sc_publishcontact $::mycontact
		if { $mode == {g} } {
			set ::mode $mode
			ml_grouphead [lindex [array get ::jgroups $id] end]
		} elseif { $mode == {p} } {
			set ::mode $mode
			ml_personhead [lsearch -inline [array get ::buddies] "$id:*"]
		}
	}

	# array names
	#
	#array set ::group_to_sig {}
	#array set ::group_to_sigreq {}
	#array set ::person_to_sig {}
	#array set ::person_to_sigreq {}
	#
	toplevel .sm
	wm title .sm "Signature manager {$mode} #$id"
	pack [panedwindow .sm.p -ori vert -width 240] -fill both -expand 1
	.sm.p add [frame ".sm.top"] -stretch never
	pack [label .sm.top.l -text "mode: {$mode} | id: <$id>" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	.sm.p add [frame ".sm.req"] -stretch always 
	pack [scrollbar .sm.req.y -activebackground {#606060}  -troughcolor {#606060}  -command "tl_yview 1 .sm.req.l"] -fill y -side right
	pack [listbox .sm.req.l -listvariable ::sm_req_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 12 -yscrollc ".sm.req.y set"] -fill both -expand 1 -side right
	.sm.p add [frame ".sm.add"] -stretch always 
	pack [scrollbar .sm.add.y -activebackground {#606060}  -troughcolor {#606060}  -command "tl_yview 1 .sm.add.l"] -fill y -side right
	pack [listbox .sm.add.l -listvariable ::sm_add_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 12 -yscrollc ".sm.add.y set"] -fill both -expand 1 -side right
	.sm.p add [frame ".sm.cmd"] -stretch never
	pack [button .sm.cmd.rem -text "rem" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "rem_cmd $mode $id" ] -fill both -side left
	pack [button .sm.cmd.add -text "add" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "add_cmd $mode $id"] -fill both -side right
	ref_cmd $mode $id
}

proc show_grouplist {} {
	if { [winfo exists .g] == 1 } {
		return
	}
	toplevel .g
	wm title .g "Groups (port=$::myport) (#$::me)"
	make_menu .g	
	pack [panedwindow .g.p -ori vert -width 240] -fill both -expand 1
	.g.p add [frame ".g.l"] -stretch always 
	pack [scrollbar .g.l.y -activebackground {#606060}  -troughcolor {#606060}  -command "tl_yview 1 .g.l.l"] -fill y -side right
	pack [listbox .g.l.l -listvariable ::jgroups_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 12 -yscrollc ".g.l.y set"] -fill both -expand 1 -side right
	.g.p add [frame ".g.mi"] -stretch never
	pack [label .g.mi.mpl -text "<" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .g.mi.me -textvar ::me -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left 
	pack [label .g.mi.mpr -text ">" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	.g.p add [frame ".g.me"] -stretch never
	pack [label .g.me.l -textvar ::menick -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -expand 1 -side left
	.g.p add [frame ".g.b"] -stretch never
	bind .g.l.l <1> {+ show_gchatwindow [lindex $::jgroups_i [lindex [.g.l.l nearest %y] 0]]}
	bind .g.l.l <3> {+ show_group_details [lindex [array get ::groups [lindex $::jgroups_i [lindex [.g.l.l nearest %y] 0]]] end]}
	bind .g.me.l <1> {+ tk_popup .nmenu %X %Y}
	update_groups
}

proc show_buddylist {} {
	if { [winfo exists .b] == 1 } {
		return
	}
	toplevel .b
	wm title .b "Buddies (port=$::myport) (#$::me)"
	make_menu .b
	pack [panedwindow .b.p -ori vert -width 240] -fill both -expand 1
	#.b.p add [frame ".b.o"] -stretch never
	#pack [button .b.o.nws -text "nws" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_mail}] -fill both -side left 
	#pack [button .b.o.sol -text "sol" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {sol_store}] -fill both -side left 
	#pack [button .b.o.dbg -text "dbg" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_debug}] -fill both -side right
	#pack [button .b.o.die -text "kill" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {write_all;exit}] -fill both -side right 
	#.b.p add [frame ".b.mn"] -stretch never
	#pack [label .b.mn.mn -textvar ::menick -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left 
	#pack [button .b.mn.ext -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {destroy .b}] -fill both -side right
	#pack [button .b.mn.dir -textvariable ::menick  -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {tk_popup .b.menu.c 0 0}] -fill both -expand 1 -side right 
	.b.p add [frame ".b.l"] -stretch always 
	pack [scrollbar .b.l.y -activebackground {#606060}  -troughcolor {#606060}  -command "tl_yview 1 .b.l.l"] -fill y -side right
	#pack [listbox .b.l.i -listvariable ::buddies_i -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 12 -width 2 -yscrollc ".b.l.y set"] -fill y -side right
	pack [listbox .b.l.l -listvariable ::buddies_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 12 -yscrollc ".b.l.y set"] -fill both -expand 1 -side right
	#pack [treectrl .b.l.tv -height 12 -yscrollc ".b.l.y set"] -fill both -expand 1 -side right
	.b.p add [frame ".b.mi"] -stretch never
	pack [label .b.mi.mpl -text "<" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .b.mi.me -textvar ::me -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left 
	pack [label .b.mi.mpr -text ">" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	.b.p add [frame ".b.b"] -stretch never
	#pack [button .b.b.cht -text "cht" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_chatwindow [lindex $::buddies_k [lindex [.b.l.l index active] 0]]}] -fill none -side right
	#pack [button .b.b.eml -text "eml" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {set ::mode {m} ; show_editor [lindex [array get ::buddies [lindex $::buddies_k [lindex [.b.l.l index active] 0]]] 1]}] -fill none -side right
	#pack [button .b.b.del -text "del" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_buddy_del [lindex $::buddies_k [lindex [.b.l.l index active] 0]]}] -fill both -side left
	#pack [button .b.b.dir -text "dir" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_directory}] -fill both -side left
	#pack [button .b.b.dnl -text "dnl" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_downloads}] -fill both -side left
	pack [label .b.b.mn -textvar ::menick -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -justify left] -fill both -expand 1 -side left 
	#pack [button .b.b.dtl -text "dtl" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {
	#	show_buddy_details [lindex $::buddies_k [lindex [.b.l.l index active] 0]]
	#}] -fill none -side left
	pack [label .b.b.bs -textvariable ::peernum -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -justify right] -fill both -side right 
	bind .b.l.l <1> {+ show_chatwindow [lindex $::buddies_k [lindex [.b.l.l nearest %y] 0]]}
	bind .b.l.l <3> {+ show_buddy_details [lindex $::buddies_k [lindex [.b.l.l nearest %y] 0]]}
	bind .b.b.mn <1> {+ tk_popup .nmenu %X %Y}
	update_buddies
}

proc show_gmailwindow {gid} {
	if { [llength $gid] > 1 || $gid == "" } {
		return
	}
	if { [winfo exists ".gm_$gid"] == 1 } {
		return
	}
	toplevel ".gm_$gid"	
	wm title ".gm_$gid" "Mail in group [string range [dict get [ml_groupdict [lindex [array get ::jgroups $gid] end]] name] 0 12] <$gid>"

	pack [panedwindow ".gm_$gid.p" -ori ver] -fill both -expand 1

	# that's for reading
	".gm_$gid.p" add [frame ".gm_$gid.r"] -stretch never
	pack [button ".gm_$gid.r.v" -text "read" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "gmail_msg_read $gid"] -fill both -side left
	#pack [button ".gm_$gid.r.d" -text "delete" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "gmail_deletebutton $gid"] -fill both -side left
	pack [button ".gm_$gid.r.r" -text "reply" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "gmail_replybutton $gid"] -fill both -side left
	pack [button ".gm_$gid.r.c" -text "compose" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "gmail_toggle_compose $gid"] -fill both -side left
	pack [entry ".gm_$gid.r.i" -textvariable "::gmail_entry_$gid" -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::listfont ] -fill both -expand 1 -side left
	pack [button ".gm_$gid.r.f" -text "f" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "gmail_history_read $gid"] -fill both -side left

	# that's for composition
	".gm_$gid.p" add [frame ".gm_$gid.c"] -stretch never -hide true
	pack [button ".gm_$gid.c.s" -text "send" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "gmail_send $gid ; gmail_toggle_compose $gid"] -fill both -side left
	pack [button ".gm_$gid.c.c" -text "cancel" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "gmail_toggle_compose $gid"] -fill both -side left
	pack [label ".gm_$gid.c.l" -text "Subject:" -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font ] -fill both -side left
	pack [entry ".gm_$gid.c.subject" -textvariable "::gmail_subject_$gid" -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::listfont ] -fill both -expand 1 -side left

	# list, hide when composing
	".gm_$gid.p" add [frame ".gm_$gid.h"] -stretch always
	pack [scrollbar ".gm_$gid.h.y" -activebackground {#606060}  -troughcolor {#606060}  -command ".gm_$gid.h.l yview"] -fill y -side right
	pack [listbox ".gm_$gid.h.l" -listvariable "::gmail_list_$gid" -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont -yscrollc ".gm_$gid.h.y set" -height 10 ] -fill both -expand 1 -side right

	# that's for reading
	".gm_$gid.p" add [frame ".gm_$gid.o"] -stretch always
	pack [scrollbar ".gm_$gid.o.y" -activebackground {#606060}  -troughcolor {#606060}  -command ".gm_$gid.o.t yview"] -fill y -side right
	pack [text ".gm_$gid.o.t" -wrap word -yscrollc ".gm_$gid.o.y set" \
		-selectforeground {#6090c0} -selectbackground {#606060} \
		-highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} \
		-padx 5 -pady 3 -height 10 -width 80 -font $::listfont] \
		-fill both -expand 1 -side right
	
	# that's for composition
	".gm_$gid.p" add [frame ".gm_$gid.i"] -stretch always -hide true
	pack [scrollbar ".gm_$gid.i.y" -activebackground {#606060}  -troughcolor {#606060}  -command ".gm_$gid.i.t yview"] -fill y -side right
	pack [text ".gm_$gid.i.t" -wrap word -yscrollc ".gm_$gid.i.y set" \
		-selectforeground {#6090c0} -selectbackground {#606060} \
		-highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} \
		-padx 5 -pady 3 -height 21 -width 80 -font $::listfont] \
		-fill both -expand 1 -side right

	".gm_$gid.p" add [frame ".gm_$gid.t"] -stretch never
	pack [label ".gm_$gid.t.l" -text "$::menick <[string range $::me 0 15]...> in [string range [dict get [ml_groupdict [lindex [array get ::jgroups $gid] end]] name] 0 12] <[string range $gid 0 15]...>" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font] -fill both -side left


	gmail_history_read $gid
	set cmd [concat {+ click_gmailwindow} ".gm_$gid.o.t" $gid {%x %y}]
	".gm_$gid.o.t" tag bind filelink <1> $cmd
	bind ".gm_$gid.r.i" <Key-Return> "gmail_history_read $gid"
	bind ".gm_$gid.h.l" <Key-Return> "gmail_msg_read $gid"
}

proc gmail_toggle_compose {gid} {
	mail_toggle_compose_common ".gm_$gid"
}

proc mail_toggle_compose {hash} {
	mail_toggle_compose_common ".cm_$hash"
}

proc mail_toggle_compose_common {w} {
	if { [ "$w.p" panecget "$w.r" -hide ] } {
		"$w.p" paneconfigure "$w.r" -hide false
		"$w.p" paneconfigure "$w.c" -hide true
		"$w.p" paneconfigure "$w.o" -hide false
		"$w.p" paneconfigure "$w.h" -hide false
		"$w.p" paneconfigure "$w.i" -hide true
	} else {
		"$w.p" paneconfigure "$w.r" -hide true
		"$w.p" paneconfigure "$w.c" -hide false
		"$w.p" paneconfigure "$w.o" -hide true
		"$w.p" paneconfigure "$w.h" -hide true
		"$w.p" paneconfigure "$w.i" -hide false
	}
}

proc gmail_send {gid} {
	upvar 0 "::gmail_subject_$gid" gmail_subject
	set kfrom "[::pki::public_key $::mekey]"
	set kto {}
	set hfrom $::me
	set from "[string range $::menick 0 64] <$::me>"
	set subject "[string range $gmail_subject 0 64]"
	set epoch [clock seconds]

	set g [ml_groupdict $::jgroups($gid)]
	set group $gid
	set hto $gid
	set to "[string range [dict get $g name] 0 64] <$gid>" 
	set hsubject "[string range $gmail_subject 0 64]"
	set type "g"

	set body [string range [".gm_$gid.i.t" get 1.0 end] 0 4096]

	set whole {}
	append whole "From:\t$from\n"
	append whole "To:\t$to\n"
	append whole "Subject:\t$subject\n"
	append whole "Epoch:\t$epoch\n"
	append whole "\n"
	append whole "$body\n\n"

	puts "Formed message:"
	puts $whole

	set whole [encoding convertto utf-8 $whole]	

	set len [string length $whole]
	set hash [::sha1::sha1 $whole]

	set h {}
	dict set h hash $hash 
	dict set h group $group
	dict set h len $len
	dict set h epoch $epoch
	dict set h from $hfrom
	dict set h to $hto 
	dict set h subject $hsubject
	dict set h type $type
	dict set h nickname $::menick
	dict set h kfrom $kfrom
	dict set h kto $kto 
	dict set h gsig {} 
	set header [dict_to_header $h] 
	puts "Formed header:"
	puts $header

	set path [file join $::filepath "mailnews" $hash]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	puts -nonewline $fchan $whole	
	flush $fchan
	close $fchan

	ml_add_hdrs $gid $header
	set "::gmail_subject_$gid" {}
	".gm_$gid.i.t" delete 1.0 end
}

proc gmail_replybutton {gid} {
	upvar 0 "::gmail_list_k_$gid" hlist_vn
	set hlist_i [lindex [ ".gm_$gid.h.l" index active ] 0]
	set hdr [lindex [split $hlist_vn] $hlist_i ]
	if { $hdr == "" } {
		return
	}
	set h [header_to_dict $hdr]
	if { $h == "" } {
		return
	}
	set subj [dict get $h subject]
	gmail_toggle_compose $gid
	set "::gmail_subject_$gid" "Re:$subj"
}

proc gmail_msg_read {gid} {
	set w ".gm_$gid.o.t"
	upvar 0 "::gmail_list_k_$gid" hlist_vn
	set hlist_i [lindex [ ".gm_$gid.h.l" index active ] 0]
	set hdr [lindex [split $hlist_vn] $hlist_i ]
	if { $hdr == "" } {
		puts "no header"
		return
	}
	set h [header_to_dict $hdr]
	if { $h == "" } {
		puts "bad header"	
		return
	}

	$w delete 1.0 end

	set hash [dict get $h hash]
	set type [dict get $h type]

	if { $type == "g" } {
		set body [ml_get_eml $hash]
		set comment "group message"
	} else {
		set body "oopsie"
		set comment "oopsie"
	}

	if { $body == -1 || $body == "" } {
		return
	}

	set body [encoding convertfrom utf-8 $body]	
	set lines [split $body "\n"]
	set from [lindex $lines 0]
	set to [lindex $lines 1]
	set subject [lindex $lines 2]
	set epoch [lindex $lines 3]
	set bodylines [lrange $lines 5 end]
	$w tag configure red -foreground {#c06060} 
	$w tag configure cyan -foreground {#6090c0}
	$w tag configure blue -foreground {#6060c0}
	$w tag configure headerlink -foreground {#6060c0} -underline true
	$w tag configure mlfilelink -foreground {#6060c0} -underline true
	$w tag configure attachlink -foreground {#c06060} -underline true
	$w tag configure yellow -foreground {#c09060}
	$w tag configure magenta -foreground {#c060c0}
	$w tag configure hide -elide true
	#$w insert end "Comment: $comment\n" {red}
	#$w insert end "   Sent: [clock format [dict get $h epoch] -format {%Y-%m-%d %H:%M:%S}]\n" {red}
	$w insert end "Comment: $comment ; [clock format [dict get $h epoch] -format {%Y-%m-%d %H:%M:%S}]\n" {red}
	$w insert end "   $from\n" {cyan}
	$w insert end "     $to\n" {cyan}
	$w insert end "$subject\n" {cyan}
	$w insert end "\n"
	foreach line $bodylines {
		if { [lindex [split $line { }] 0] == "FILE" } {
			set tag {blue mlfilelink}
		} elseif { [lindex [split $line { }] 0] == "HDR" } {
			$w insert end "$line " {hide headerlink} 
			set nline "[lrange $line 2 end] hash://[lindex [split [lindex $line 1] {:}] 0]\n"
			set line $nline
			set tag {headerlink}
		} elseif { [lindex [split $line { }] 0] == "ATTACH" } {
			#.m.f.t insert end "$line " {hide attachlink}
			set nline "[string trim [lindex [split $line {()}] 0]]\n"
			#set line $nline
			set line {}
			set tag {attachlink}
		} elseif { [string index $line 0] == ">" } {
			set tag {magenta}
		} else {
			set tag {} 
		}
		$w insert end "$line\n" $tag
	}
	$w insert end "\n"
}

proc gmail_history_read {gid} {
	set "::gmail_list_$gid" {}
	set "::gmail_list_k_$gid" {}
	puts "filling gmail_header_list"
	upvar 0 "::gmail_entry_$gid" gmail_entry
	set sf [lindex $gmail_entry end]
	if { $sf != "" } {
		set reg [string map {{ } {.*}} $sf]
	} else {
		set reg {.*}
	}
	puts "regex is $reg"
	set sorted {}
	foreach hdr [lrange [ml_get_hdrs $gid] 2 end] {
		set h [header_to_dict $hdr]	
		if { $h == "" } {
			continue
		}
		if { [regexp $reg $h] == 0 } {
			continue	
		}
		set t "[clock format [dict get $h epoch] -format {%Y-%m-%d %H:%M:%S}] | [string range [dict get $h from] 0 3] | [string range [dict get $h nickname] 0 11] | [string range [dict get $h hash] 0 3] | [string range [dict get $h subject] 0 31] ([dict get $h len])"
		lappend sorted $t $hdr [dict get $h epoch]
	}
	foreach {title hdr epoch} [lsort -decreasing -stride 3 -index end $sorted] {
		lappend "::gmail_list_$gid" $title
		lappend "::gmail_list_k_$gid" $hdr
	}
}

proc show_gchatwindow {gid} {
	if { [llength $gid] > 1 || $gid == "" } {
		return
	}
	if { [winfo exists ".g_$gid"] == 1 } {
		return
	}
	#if { $::mode != {g} } {
	#	#return
	#	set ::mode {g}
	#}
	#ml_grouphead $::jgroups($gid)
	array unset ::gnotices "$gid*"
	set sources [lrange [ml_get_srcs $gid] 2 end]
	set "::ml_src_$gid" {}
	foreach src $sources {
		set nickname {}
		set s_sort {}
		set contacts [lsearch -all -inline [array get ::contacts] "$src:*"]
		foreach contact $contacts {
			set c [contact_to_dict $contact]
			if { $c != "" && [dict get $c peerid] == $src} {
				set nickname [dict get $c nickname]
				set epoch [dict get $c epoch]
				lappend s_sort "$nickname" $epoch
			} else {
				lappend s_sort "n/a" 0
			}
		}
		set nickname [lindex [lsort -integer -stride 2 -index 1 -decreasing $s_sort] 0]
		lappend "::ml_src_$gid" "$nickname <$src>"
	}
	set gpeerid [dict get [ml_groupdict $::jgroups($gid)] peerid]
	if { $gpeerid == $::me } {
		set comment "(host is me)"
	} else {
		set comment "(client)"
	}
	toplevel ".g_$gid"	
	wm title ".g_$gid" "Chat with [string range [dict get [ml_groupdict [lindex [array get ::jgroups $gid] end]] name] 0 12] <$gid>"
	pack [panedwindow ".g_$gid.p" -ori hor] -fill both -expand 1
	".g_$gid.p" add [panedwindow ".g_$gid.l" -ori ver] -stretch always
	".g_$gid.l" add [frame ".g_$gid.t"] -stretch never
	pack [label ".g_$gid.t.l" -text "$::menick <[string range $::me 0 15]...> to [string range [dict get [ml_groupdict [lindex [array get ::jgroups $gid] end]] name] 0 12] <[string range $gid 0 15]...>" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font] -fill both -side left
	pack [label ".g_$gid.t.r" -text "$comment" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font] -fill both -side right
	".g_$gid.l" add [frame ".g_$gid.o"] -stretch always
	pack [scrollbar ".g_$gid.o.ys" -activebackground {#606060}  -troughcolor {#606060}  -command ".g_$gid.o.ls yview"] -fill y -side right
	pack [listbox ".g_$gid.o.ls" -listvariable "::ml_src_$gid" -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -yscrollc ".g_$gid.o.ys set" -width 20 ] -fill y -side right
	pack [scrollbar ".g_$gid.o.y" -activebackground {#606060}  -troughcolor {#606060}  -command ".g_$gid.o.t yview"] -fill y -side right
	pack [text ".g_$gid.o.t" -wrap word -yscrollc ".g_$gid.o.y set" \
		-selectforeground {#6090c0} -selectbackground {#606060} \
		-highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} \
		-padx 5 -pady 3 -height 18 -width 60 -font $::font] \
		-fill both -expand 1 -side right
	#".g_$gid.l" add [frame ".g_$gid.i"] -stretch never
	#pack [button ".g_$gid.i.s" -text "send" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "gchat_sendbutton $gid"] -fill both -side right
	#pack [scrollbar ".g_$gid.i.y" -activebackground {#606060}  -troughcolor {#606060}  -command ".g_$gid.i.t yview"] -fill y -side right
	#pack [text ".g_$gid.i.t" -wrap word -yscrollc ".g_$gid.i.y set" \
	#	-selectforeground {#6090c0} -selectbackground {#606060} \
	#	-highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} \
	#	-padx 5 -pady 3 -height 4 -width 80 -font $::font] \
	#	-fill both -expand 1 -side right
	#".g_$gid.p" add [panedwindow ".g_$gid.bl" -ori ver] -stretch always
	".g_$gid.l" add [frame ".g_$gid.r"] -stretch never
	pack [entry ".g_$gid.r.i" -textvariable ::gchat_entry -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font ] -fill both -expand 1 -side left
	pack [button ".g_$gid.r.s" -text "send" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "gchat_sendbutton $gid"] -fill both -side right
	pack [button ".g_$gid.r.f" -text "file" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "show_fileoffer g $gid"] -fill both -side right 
	pack [button ".g_$gid.r.in" -text "insert" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "insert_single_file g $gid"] -fill both -side right 
	#pack [button ".g_$gid.r.img" -text "image" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "insert_inline_image g $gid"] -fill both -side right 
	#gchat_history_append $gid
	after idle [list gchat_history_read $gid]
	if { $::group_sync_allowed == 1 } {
		after idle [list gchat_send $gid [wrap "$::menick:$::me"] {} "SYNC" {}]
	}
	set cmd [concat {+ click_gchatwindow} ".g_$gid.o.t" $gid {%x %y}]
	".g_$gid.o.t" tag bind filelink <1> $cmd
	#bind ".g_$gid.i.t" <Key-Return> "gchat_sendbutton $gid"
	bind ".g_$gid.r.i" <Key-Return> "gchat_sendbutton $gid"
}

proc click_gchatwindow {w gid x y} {
	set range [$w tag prevrange filelink [$w index @$x,$y]]
	set filelink [eval $w get $range]
	if { $filelink != "" } {
		puts "filelink is $filelink in group $gid" 
		after idle [list show_filedialog $filelink {}]
	}
}

proc show_chatwindow {hash} {
	if { [llength $hash] > 1 || $hash == "" } {
		return
	}
	if { [winfo exists ".c_$hash"] == 1 } {
		return
	}
	#if { $::mode != {p} } {
	#	set ::mode {p}
	#	#return
	#}
	#ml_personhead $::buddies($hash)
	array unset ::notices "$hash*"
	after 50 update_buddies
	toplevel ".c_$hash"
	wm title ".c_$hash" "Chat with [dict get [contact_to_dict [lindex [array get ::buddies $hash] 1]] nickname] <[dict get [contact_to_dict [lindex [array get ::buddies $hash] 1]] peerid]>"
	pack [panedwindow ".c_$hash.p" -ori vert] -fill both -expand 1
	".c_$hash.p" add [frame ".c_$hash.t"] -stretch never
	pack [label ".c_$hash.t.l" -text "$::menick <[string range $::me 0 15]...> to [dict get [contact_to_dict [lindex [array get ::buddies $hash] 1]] nickname] <[string range [dict get [contact_to_dict [lindex [array get ::buddies $hash] 1]] peerid] 0 15]...>" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font] -fill both -side left
	#pack [button ".c_$hash.t.x" -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "destroy .c_$hash"] -fill both -side right
	".c_$hash.p" add [frame ".c_$hash.o"] -stretch always
	pack [scrollbar ".c_$hash.o.y" -activebackground {#606060}  -troughcolor {#606060}  -command ".c_$hash.o.t yview"] -fill y -side right
	pack [text ".c_$hash.o.t" -wrap word -yscrollc ".c_$hash.o.y set" \
		-selectforeground {#6090c0} -selectbackground {#606060} \
		-highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} \
		-padx 5 -pady 3 -height 18 -width 80 -font $::font] \
		-fill both -expand 1 -side right
	#".c_$hash.p" add [frame ".c_$hash.i"] -stretch never
	#pack [scrollbar ".c_$hash.i.y" -activebackground {#606060}  -troughcolor {#606060}  -command ".c_$hash.i.t yview"] -fill y -side right
	#pack [text ".c_$hash.i.t" -wrap word -yscrollc ".c_$hash.i.y set" \
	#	-selectforeground {#6090c0} -selectbackground {#606060} \
	#	-highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} \
	#	-padx 5 -pady 3 -height 8 -width 80 -font $::listfont] \
	#	-fill both -expand 1 -side right
	".c_$hash.p" add [frame ".c_$hash.c"] -stretch never
	pack [entry ".c_$hash.c.i" -textvariable ::chat_entry -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font ] -fill both -expand 1 -side left
	pack [button ".c_$hash.c.s" -text "send" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "chat_sendbutton $hash"] -fill both -side right
	pack [button ".c_$hash.c.f" -text "file" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "show_fileoffer p $hash"] -fill both -side right 
	pack [button ".c_$hash.c.in" -text "insert" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "insert_single_file p $hash"] -fill both -side right 
	#pack [button ".c_$hash.c.img" -text "image" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "insert_inline_image p $hash"] -fill both -side right 
	#chat_history_append $hash
	after idle [list chat_history_read $hash]
	set cmd [concat {+ click_chatwindow} ".c_$hash.o.t" $hash {%x %y}]
	".c_$hash.o.t" tag bind filelink <1> $cmd 
	#bind ".c_$hash.i.t" <Key-Return> "chat_sendbutton $hash"
	bind ".c_$hash.c.i" <Key-Return> "chat_sendbutton $hash"
}

proc click_chatwindow {w hash x y} {
	set range [$w tag prevrange filelink [$w index @$x,$y]]
	set filelink [eval $w get $range]
	if { $filelink != "" } {
		puts "filelink is $filelink from $hash" 
		after idle [list show_filedialog $filelink $hash]
	}
}

proc show_filedialog {req buddyhash} {
	#if { [winfo exists .fd] == 1 || $header == "" } {
	#	return
	#}
	if { [winfo exists .fd] == 1 || $req == "" } {
		return
	}
	#puts "header $header"
	#set h [header_to_dict $header]
	#puts "dict $h"
	#set name [encoding convertfrom utf-8 [::base64::decode [dict get $h subject]]]
	#set chunksize [dict get $h chunksize]
	#set chunks [dict get $h chunks]
	#set size [expr {$chunks*$chunksize}]
	#set hash [dict get $h hash]
	puts "req $req"
	set r [dl_reqdict $req]
	puts "r $r"
	if { $r == "" } {
		return
	}
	if { $buddyhash == "" } {
		set buddyhash "{}"
	}
	set name [dict get $r filename]
	set hash [dict get $r filehash]
	set size [dict get $r len]
	toplevel .fd
	wm title .fd "Download file"
	pack [panedwindow .fd.p -ori vert] -fill both -expand 1
	.fd.p add [frame .fd.t] -stretch always
	pack [label .fd.t.t -text "Do you want to download file\n $name\nby hash\n $hash\nof size\n $size?" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	.fd.p add [frame .fd.b] -stretch never 
	pack [button .fd.b.c -text "cancel" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "destroy .fd"] -fill both -side left
	#pack [button .fd.b.o -text "ok" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "sc_download $header ; destroy .fd"] -fill both -side right 
	pack [button .fd.b.o -text "ok" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "dl_add $req $buddyhash {}; destroy .fd ; show_dlstate"] -fill both -side right 
}

proc show_dlstate {} {
	if { [winfo exists .dls] == 1 } {
		return
	}
	toplevel .dls
	wm title .dls "DLState"
	pack [panedwindow .dls.p -ori vert] -fill both -expand 1
	.dls.p add [frame .dls.t] -stretch never 
	#pack [button .dls.t.x -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "destroy .dls"] -fill both -side left 
	.dls.p add [frame .dls.l] -stretch always
	pack [scrollbar .dls.l.y -activebackground {#606060}  -troughcolor {#606060}  -command "tl_yview 6 .dls.l.n .dls.l.s .dls.l.p .dls.l.h .dls.l.b .dls.l.st"] -fill y -side right
	pack [listbox .dls.l.n -listvariable ::dlstate_name -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 24 -width 32 -yscrollc ".dls.l.y set"] -fill both -expand 1 -side left
	pack [listbox .dls.l.s -listvariable ::dlstate_size -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 24 -width 10 -yscrollc ".dls.l.y set"] -fill y -side left
	pack [listbox .dls.l.p -listvariable ::dlstate_perc -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 24 -width 6 -yscrollc ".dls.l.y set"] -fill y -side left
	pack [listbox .dls.l.h -listvariable ::dlstate_hash -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 24 -width 12 -yscrollc ".dls.l.y set"] -fill y -side left
	#pack [listbox .dls.l.b -listvariable ::dlstate_buddy -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 24 -width 12 -yscrollc ".dls.l.y set"] -fill y -side left
	pack [listbox .dls.l.st -listvariable ::dlstate_state -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 24 -width 8 -yscrollc ".dls.l.y set"] -fill y -side left
	.dls.p add [frame .dls.b] -stretch never 
	pack [button .dls.b.start -text "start" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {dl_start [lindex $::dlstate_hash [lindex [.dls.l.n index active] 0]]}] -fill both -side right
	pack [button .dls.b.stop -text "stop" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {dl_stop [lindex $::dlstate_hash [lindex [.dls.l.n index active] 0]]}] -fill both -side right
	pack [button .dls.b.delete -text "delete" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {dl_del [lindex $::dlstate_hash [lindex [.dls.l.n index active] 0]]}] -fill both -side right
	update_dlstate
}

proc update_dlstate {} {
	if { [winfo exists .dls] == 0 } {
		return
	}
	set ::dlstate {}
	set ::dlstate_name {}
	set ::dlstate_size {}
	set ::dlstate_perc {}
	set ::dlstate_hash {}
	set ::dlstate_buddy {}
	set ::dlstate_state {}
	foreach {hash detail} [array get ::dl_by_hash] {
		set size [dict get $detail len]
		set top $::dlstate_by_hash($hash,top)
		if { $top >= $size } {
			set top $size
		}
		set buddyhash [lindex [array get ::dlstate_by_hash $hash,buddy] end]
		lappend ::dlstate_name [dict get $detail filename]
		lappend ::dlstate_size $size
		lappend ::dlstate_perc "[expr {int((100.0*$top)/(1.0*$size))}]%"
		lappend ::dlstate_hash $hash
		if { $buddyhash != {} } {
			lappend ::dlstate_buddy [dict get [contact_to_dict $::buddies($buddyhash)] nickname]
		} else {
			lappend ::dlstate_buddy "-" 
		}
		lappend ::dlstate_state $::dlstate_by_hash($hash,state)
	}
	after 1000 update_dlstate
}

proc tl_yview args {
	set num [lindex $args 0]	
	set lists [lrange $args 1 $num]	
	set a [lrange $args [expr {$num+1}] end]
	foreach l $lists {
		$l yview {*}$a
	}
}

proc show_fileoffer {mode hash} {
	if {[winfo exists .fo] == 1} {
		return
	}
	toplevel .fo
	wm title .fo "File offer"
	pack [panedwindow .fo.p -ori vert] -fill both -expand 1
	.fo.p add [frame .fo.t] -stretch never
	pack [label .fo.t.l -text "Choose file to send to chat ($mode) #$hash :" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	#pack [button .fo.t.x -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "destroy .fo"] -fill both -side right
	.fo.p add [frame .fo.ll] -stretch never
	pack [label .fo.ll.name -text "name" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -expand 1 -side left 
	pack [label .fo.ll.size -text "size" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 8 ] -fill y -side left
	pack [label .fo.ll.hash -text "hash" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 12 ] -fill y -side left
	.fo.p add [frame .fo.l] -stretch always
	pack [listbox .fo.l.name -listvariable ::fo_name -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -yscrollc ".fo.l.y set"] -fill both -expand 1 -side left
	pack [listbox .fo.l.size -listvariable ::fo_size -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -yscrollc ".fo.l.y set" -width 8] -fill y -side left
	pack [listbox .fo.l.hash -listvariable ::fo_hash -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -yscrollc ".fo.l.y set" -width 12] -fill y -side left
	pack [scrollbar .fo.l.y -activebackground {#606060}  -troughcolor {#606060}  -command {tl_yview 3 .fo.l.name .fo.l.size .fo.l.hash}] -fill y -side right
	.fo.p add [frame .fo.b] -stretch never
	pack [button .fo.b.s -text "send" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "send_fileoffer $mode $hash; destroy .fo"] -fill both -side right

	set ::fo_name {}
	set ::fo_size {}
	set ::fo_hash {}
	set ::fo_header {}
	foreach {fhash details} [array get ::file_by_hash] {
		array set ::sources [list $fhash $::me]
		set name [unwrap [dict get $details name]]
		set size [dict get $details size]
		lappend ::fo_name $name
		lappend ::fo_size $size
		lappend ::fo_hash $fhash
		#set h {}
		#dict set h hash $fhash
		#dict set h epoch [clock seconds]
		#dict set h chunksize $::default_chunksize
		#dict set h chunks [expr {1+$size/$::default_chunksize}] 
		#dict set h from {}
		#dict set h to {}
		#dict set h nickname {} 
		#dict set h subject $name 
		#dict set h type {}
		#dict set h kfrom {}
		#dict set h kto {}
		#lappend ::fo_header "[dict_to_header $h]"
		set r {}
		dict set r filehash $fhash
		dict set r filename $name
		dict set r offset 0
		dict set r len $size
		lappend ::fo_header "[dl_dictreq $r]"
	}
	proc send_fileoffer {mode hash} {
		if { $hash == {} } {
			return
		}
		set notice "FILE [lindex $::fo_name [lindex [.fo.l.name index active] 0]] ([lindex $::fo_size [lindex [.fo.l.name index active] 0]]) <[lindex $::fo_header [lindex [.fo.l.name index active] 0]]>"
		if { $mode == {p} } {
				chat_notice $hash $notice
				chat_append_notice $hash [clock seconds] $notice
				chat_history $hash [clock seconds] $notice 0 {}
		} elseif { $mode == {g} } {
				gchat_notice $hash $notice
				gchat_append_notice $hash [wrap $::menick] [clock seconds] $notice
				gchat_history $hash [wrap "$::menick:$::me"] [clock seconds] $notice 0 {}
		}
	}
}	

proc insert_single_file {mode id} {
	set filename [tk_getOpenFile]
	if { $filename != "" && [file exists $filename] } { 
		hash_file $filename
	} else {
		return
	}
	set hash [lindex [array get ::hash_by_file [wrap $filename]] end]
	puts "hash $hash"
	set details [lindex [array get ::file_by_hash $hash] end]
	puts "details $details"
	set name [lindex [file split [unwrap [dict get $details name]]] end]
	set size [dict get $details size]
	set r {}
	dict set r filehash $hash
	dict set r filename $name
	dict set r offset 0
	dict set r len $size
	set req [dl_dictreq $r]
	if { $req == "" } {
		return
	}
	set notice "FILE $name ($size) <$req>"
	if { $mode == {p} } {
			chat_notice $id $notice
			chat_append_notice $id [clock seconds] $notice
			chat_history $id [clock seconds] $notice 0 {}
	} elseif { $mode == {g} } {
			gchat_notice $id $notice
			gchat_append_notice $id [wrap $::menick] [clock seconds] $notice
			gchat_history $id [wrap "$::menick:$::me"] [clock seconds] $notice 0 {}
	} elseif { $mode == {e} } {
			.e.x.t insert end "\n$notice\n" {blue mlfilelink}	
	}
}

proc attach_file {} {
	set filename [tk_getOpenFile]
	set data {}
	if { $filename != "" && [file exists $filename] } { 
		hash_file $filename
		set f [open $filename r]
		fconfigure $f -translation binary -encoding binary
		set data [read $f]
		close $f
	} else {
		return
	}
	set hash [lindex [array get ::hash_by_file [wrap $filename]] end]
	set details [lindex [array get ::file_by_hash $hash] end]
	set name [lindex [file split [unwrap [dict get $details name]]] end]
	set size [dict get $details size]
	set vnotice "ATTACH $name ($size) "
	set dnotice "<[wrap $data]>"
	.e.x.t insert end "\n"
	.e.x.t insert end "$vnotice" {red attachlink}
	.e.x.t insert end "$dnotice" {hide red attachlink}
	.e.x.t insert end "\n"
}

proc insert_inline_image {mode id} {
	#set types {
	#	{{GIF} {.gif}}
	#	{{GIF} {.GIF}}
	#	{{PNG} {.png}}
	#	{{PNG} {.PNG}}
	#	{{JPG} {.jpg}}
	#	{{JPG} {.JPG}}
	#	{{JPG} {.jpeg}}
	#	{{JPG} {.JPEG}}
	#}
	set types {
		{{PNG} {.png}}
		{{PNG} {.PNG}}
	}
	set filename [tk_getOpenFile -filetypes $types]
	if { $filename == "" || ![file exists $filename] } { 
		return
	} else {
		hash_file $filename
	}
	set name [lindex [file split $filename] end]
	puts "reading $filename"
	set f [open $filename r]
	fconfigure $f -translation binary -buffering none
	set data [read $f]
	close $f
	puts "have read $filename"
	set size [string length $data]
	if { $size > [expr "1024*1024*8"] } {
		puts "bigger than 8 mb, not sending"
		return
	}
	set notice "IMG $name ($size) <[::base64::encode -maxlen 0 $data]>"
	puts "formed notice $name ($size) <...>"
	if { $mode == {p} } {
			chat_notice $id $notice
			puts "scheduled notice"
			chat_append_notice $id [clock seconds] $notice
			puts "appended notice"
			#chat_history $id [clock seconds] $notice 0 {}
			#puts "write to history"
	} elseif { $mode == {g} } {
			gchat_notice $id $notice
			puts "scheduled notice"
			gchat_append_notice $id [wrap $::menick] [clock seconds] $notice
			puts "appended notice"
			#gchat_history $id [wrap "$::menick:$::me"] [clock seconds] $notice 0 {}
			#puts "write to history"
	}
}

proc show_debug {} {
	if { [winfo exists .p] == 1 } {
		return
	}
	toplevel .p
	wm title .p {Debug console}
	pack [panedwindow .p.p -ori vert] -fill both -expand 1
	.p.p add [frame .p.x0]
	grid [label .p.x0.lmp -textvar ::myport -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 1 -row 0
	grid [label .p.x0.lme -textvar ::me -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 3 -rowspan 1 -column 2 -row 0
	#grid [button .p.x0.x -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {destroy .p}] -columnspan 1 -rowspan 1 -column 0 -row 0
	.p.p add [frame .p.x1]
	grid [frame .p.x1.p ] -columnspan 1 -rowspan 10 -column 0 -row 0
	grid [label .p.x1.p.t -text "processes" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 0 -row 0
	grid [listbox .p.x1.p.l -listvariable ::p_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font ] -columnspan 1 -rowspan 10 -column 0 -row 1 
	grid [frame .p.x1.b ] -columnspan 1 -rowspan 10 -column 1 -row 0
	grid [label .p.x1.b.t -text "buckets" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 0 -row 0
	grid [listbox .p.x1.b.l -listvariable ::b_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font ] -columnspan 1 -rowspan 10 -column 0 -row 1
	grid [frame .p.x1.ps ] -columnspan 1 -rowspan 10 -column 2 -row 0
	grid [label .p.x1.ps.t -text "peerstore" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 0 -row 0
	grid [listbox .p.x1.ps.l -listvariable ::peerstore_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font ] -columnspan 1 -rowspan 10 -column 0 -row 1
	grid [frame .p.x1.vs ] -columnspan 1 -rowspan 10 -column 3 -row 0
	grid [label .p.x1.vs.t -text "valuestore" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 0 -row 0
	grid [listbox .p.x1.vs.l -listvariable ::valuestore_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font ] -columnspan 1 -rowspan 10 -column 0 -row 1
	grid [frame .p.x1.vc ] -columnspan 1 -rowspan 10 -column 4 -row 0
	grid [label .p.x1.vc.t -text "contacts" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 0 -row 0
	grid [listbox .p.x1.vc.l -listvariable ::contacts_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font ] -columnspan 1 -rowspan 10 -column 0 -row 1
	grid [frame .p.x1.vh ] -columnspan 1 -rowspan 10 -column 5 -row 0
	grid [label .p.x1.vh.t -text "headers" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 0 -row 0
	grid [listbox .p.x1.vh.l -listvariable ::headers_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font ] -columnspan 1 -rowspan 10 -column 0 -row 1
	grid [frame .p.x1.vw ] -columnspan 1 -rowspan 10 -column 6 -row 0
	grid [label .p.x1.vw.t -text "waitvalue" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 0 -row 0
	grid [listbox .p.x1.vw.l -listvariable ::waitvalue_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font ] -columnspan 1 -rowspan 10 -column 0 -row 1
	.p.p add [frame .p.x2]
	grid [label .p.x2.brl_h -text {host} -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 0 -row 0 
	grid [label .p.x2.brl_p -text {port} -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 1 -row 0
	grid [label .p.x2.brl_k -text {key} -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 2 -row 0
	grid [label .p.x2.brl_v -text {value} -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 3 -row 0
	grid [entry .p.x2.brt_h -textvariable ::formhost -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 0 -row 1 
	grid [entry .p.x2.brt_p -textvariable ::formport -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 1 -row 1
	grid [entry .p.x2.brt_k -textvariable ::formkey -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 2 -row 1
	grid [entry .p.x2.brt_v -textvariable ::formvalue -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font ] -columnspan 1 -rowspan 1 -column 3 -row 1
	.p.p add [frame .p.x3]
	grid [button .p.x3.br -text "sol" -command {sol $::formhost $::formport} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font ] -columnspan 1 -rowspan 1 -column 0 -row 0
	grid [button .p.x3.bp -text "ping" -command {str_start [str_create PING $::formhost $::formport none none]} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font ] -columnspan 1 -rowspan 1 -column 1 -row 0
	grid [button .p.x3.bs -text "store" -command {str_start [str_create STORE $::formhost $::formport $::formkey $::formvalue]} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font ] -columnspan 1 -rowspan 1 -column 2 -row 0
	grid [button .p.x3.bfn -text "find_node" -command {str_start [str_create FIND_NODE $::formhost $::formport $::formkey none]} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font ] -columnspan 1 -rowspan 1 -column 3 -row 0
	grid [button .p.x3.bfv -text "find_value" -command {str_start [str_create FIND_VALUE $::formhost $::formport $::formkey none]} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font ] -columnspan 1 -rowspan 1 -column 4 -row 0
	grid [button .p.x3.ss -text "sol_store" -command {sol_store} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font ] -columnspan 1 -rowspan 1 -column 5 -row 0
	grid [button .p.x3.hf -text "hash_files" -command {sol_store} -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font ] -columnspan 1 -rowspan 1 -column 5 -row 0
	update_widgets
}

proc show_mail {} {
	if { [winfo exists .m] == 1 } {
		return
	}
	toplevel .m
	wm title .m "Mail | $::myport | $::menick <$::me>"
	#make_menu .m
	make_nmenu

	set browse_cmd {
		if { $::mode == {p} } {
			ml_personbrowse $::ml_cur_person
		}
	}

	set chat_cmd {
		if { $::mode == {p} && $::ml_cur_person != "" } {
			set l_b [array get ::buddies]
			set l_bn [lsearch $l_b $::ml_cur_person]
			set l_hash [lindex $l_b [expr {$l_bn-1}]]
			show_chatwindow $l_hash 
		} elseif { $::mode == {g} && $::ml_cur_group != "" } {
			set l_gid [dict get [ml_groupdict $::ml_cur_group] gid]
			show_gchatwindow $l_gid
		}
	}
	
	set mail_cmd {
		if { $::mode == {g} && $::ml_cur_group != "" } {
			set gid [dict get [ml_groupdict $::ml_cur_group] gid]
			show_gmailwindow $gid
		}
	}

	set reply_cmd {
		if { $::mode == {m} } {	
			show_editor [mail_header_to_contact [lindex $::msglist_k [lindex [.m.x.ls index active] 0]]]
			set ::e_parent [lindex $::msglist_k [lindex [.m.x.ls index active] 0]]
		} elseif { $::mode == {p} && $::ml_cur_person != "" } {	
			if { [.m.p panecget .m.x -hide] } {
				set ::e_subject "Re:[dict get [header_to_dict $::ml_cur_header] subject]"
				set ::e_parent $::ml_cur_header
			} else {
				set ::e_subject "Re:[dict get [header_to_dict [lindex $::msglist_k [lindex [.m.x.ls index active] 0]]] subject]";
				set ::e_parent [lindex $::msglist_k [lindex [.m.x.ls index active] 0]]
			}
			show_editor {}
		} elseif { $::mode == {g} && $::ml_cur_group != "" } {	
			if { [.m.p panecget .m.x -hide] } {
				set ::e_subject "Re:[dict get [header_to_dict $::ml_cur_header] subject]"
				set ::e_parent $::ml_cur_header
			} else {
				set ::e_subject "Re:[dict get [header_to_dict [lindex $::msglist_k [lindex [.m.x.ls index active] 0]]] subject]";
				set ::e_parent [lindex $::msglist_k [lindex [.m.x.ls index active] 0]]
			}
			show_editor {}
		} elseif { $::mode == {n} } {
			set ::mode {n};
			set ::e_subject "Re:[dict get [header_to_dict [lindex $::msglist_k [lindex [.m.x.ls index active] 0]]] subject]";
			set ::e_parent [lindex $::msglist_k [lindex [.m.x.ls index active] 0]]
			show_editor [mail_header_to_contact [lindex $::msglist_k [lindex [.m.x.ls index active] 0]]]
		}
	}

	set emit_cmd {
		if { $::mode == {m} } {
			show_editor {}
		} elseif { $::mode == {p} && $::ml_cur_person != "" } {
			set ::e_subject {}
			set ::e_parent {}
			show_editor {}
		} elseif { $::mode == {g} && $::ml_cur_group != "" } {
			set ::e_subject {}
			set ::e_parent {}
			show_editor {}
		} elseif { $::mode == {n} }  {
			set ::mode {n};
			show_editor {}	
		}
	}

	set delete_cmd {
		if { $::mode == {p} || $::mode == {g} } {	
			if { [.m.p panecget .m.x -hide] && $::ml_cur_header != {} } {
				set hash [dict get [header_to_dict $::ml_cur_header] hash]
			} else {
				set hash [dict get [header_to_dict [lindex $::msglist_k [lindex [.m.x.ls index active] 0]]] hash]
			}
			set choice [tk_dialog .mdd "Delete message" "Do you want\nto delete message\n$hash\n?" {} 0 "Cancel" "OK"]
			if { $choice == 1 } {
				ml_add_del $hash
				.m.p paneconfigure .m.x -hide false
				.m.p paneconfigure .m.f -hide true
			}
		}
	}

	set search_cmd {
		if { $::mode == {m} } {
			.m.b.gtr configure -state disabled;
			.m.b.gtrs configure -state normal;
			show_headers [sc_get_headers [prep_header_keys [build_personal_filter]]]
		} elseif { $::mode == {n} && $::searchfield != {} }  {
			.m.b.gtr configure -state disabled;
			.m.b.gtrs configure -state normal;
			show_headers [sc_get_headers [prep_header_keys $::searchfield]]
		} elseif { $::mode == {g} && $::ml_cur_group != {} }  {
			ml_grouphead $::ml_cur_group
		} elseif { $::mode == {p} && $::ml_cur_person != {} }  {
			ml_personhead $::ml_cur_person
		}
	}

	set filter_cmd {
		if { $::mode == {g} && $::ml_cur_group != {} } {
			ml_showlist [dict get [ml_groupdict $::ml_cur_group] gid]
		} elseif { $::mode == {p} && $::ml_cur_person != {} } {
			ml_showpersonlist [dict get [contact_to_dict $::ml_cur_person] peerid]
		}
	}

	set read_cmd {
		if { $::mode == {m} || $::mode == {n} } {
			show_text [sc_download [lindex $::msglist_k [lindex [.m.x.ls index active] 0]]]
		} elseif { $::mode == {g} || $::mode == {p} } {
			set ::ml_cur_header [lindex $::msglist_k [lindex [.m.x.ls index active] 0]]
			ml_showmsg $::ml_cur_header
			if { [.m.p panecget .m.x -hide] } {
				.m.p paneconfigure .m.x -hide false
				.m.p paneconfigure .m.f -hide true
			} else {
				.m.p paneconfigure .m.f -hide false
				.m.p paneconfigure .m.x -hide true
			}
		}
	}

	pack [panedwindow .m.p -ori vert] -fill both -expand 1
	#.m.p add [frame .m.igc] -stretch never
	#pack [label .m.igc.line -textvar ::ml_topline -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left 
	.m.p add [frame .m.b] -minsize 24 -stretch never
	pack [button .m.b.gt -text "read" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command $read_cmd] -fill both -side left
	pack [button .m.b.grr -text "reply" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "$reply_cmd ; $filter_cmd" ] -fill both -side left
	pack [button .m.b.vn -text "compose" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command $emit_cmd] -fill both -side left
	pack [button .m.b.del -text "delete" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "$delete_cmd ; $filter_cmd" ] -fill both -side left
	pack [button .m.b.scs -text "p" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {set ::mode {p} ; show_contact_selection}] -fill both -side left
	pack [button .m.b.sgs -text "g" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {set ::mode {g} ; show_group_selection}] -fill both -side left
	pack [button .m.b.chat -text "chat" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command $chat_cmd] -fill both -side left
	#pack [button .m.b.dl -text "dl" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_dlstate}] -fill both -side left
	#pack [button .m.b.dbg -text "dbg" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_debug}] -fill both -side left
	#pack [button .m.b.mail -text "mail" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command $mail_cmd] -fill both -side left
	#pack [button .m.b.browse -text "browse" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command $browse_cmd] -fill both -side left
	#pack [button .m.b.mode -text "mode" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {
		#if { $::mode == {n} } {
		#	set ::mode {m}
		#} elseif { $::mode == {m} } {
		#	set ::mode {p}
		#} elseif { $::mode == {p} } {
		#	set ::mode {g}
		#} elseif { $::mode == {g} } {
		#	set ::mode {n}
		#}
	#	if { $::mode == {g} } {
	#		set ::mode {p}
	#	} else {
	#		set ::mode {g}
	#	}
	#}] -fill both -side left
	pack [label .m.b.line -textvar ::ml_topline -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -justify center] -fill both -side left
	pack [label .m.b.menu -text "menu" -activebackground {#606060} -activeforeground {#000000} -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font ] -fill both -side right
	pack [button .m.b.gtr -text "gather" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command $search_cmd] -fill both -side right
	pack [button .m.b.upd -text "f" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command $filter_cmd] -fill both -side right
	pack [entry .m.b.g -textvariable ::searchfield -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -width 12 ] -fill both -side right
	#pack [button .m.b.gtrs -text "stop" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -state disabled -command {
	#	.m.b.gtr configure -state normal;
	#	.m.b.gtrs configure -state disabled;
	#	sc_stop $::search	
	#	}] -fill both -side right
	.m.p add [frame .m.x] -minsize 24 -stretch always
	pack [scrollbar .m.x.y -activebackground {#606060}  -troughcolor {#606060}  -command "tl_yview 1 .m.x.ls"] -fill y -side right
	pack [listbox .m.x.ls -listvariable ::msglist_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont -width 80 -height 20 -yscrollc ".m.x.y set"] -fill both -expand 1 -side right
	#.m.p add [frame .m.d] -stretch always
	#pack [scrollbar .m.d.y -activebackground {#606060}  -troughcolor {#606060}  -command ".m.d.l yview"] -fill y -side right
	#pack [listbox .m.d.l -listvariable ::dllist -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont -height 3 -yscrollc ".m.d.y set"] -fill both -expand 1 -side right
	.m.p add [frame .m.f] -minsize 24 -stretch always -hide true
	pack [scrollbar .m.f.y -activebackground {#606060}  -troughcolor {#606060}  -command ".m.f.t yview"] -fill y -side right
	pack [text .m.f.t -wrap word -yscrollc ".m.f.y set" \
		-selectforeground {#6090c0} -selectbackground {#606060} \
		-padx 5 -pady 3 -height 20 -width 80 -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont] \
		-fill both -expand 1 -side right
	.m.p add [frame .m.i] -minsize 24 -stretch never
	pack [label .m.i.lm -text "port: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .m.i.lmp -textvar ::myport -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .m.i.lmode -text " | mode: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .m.i.lmodel -text "{" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .m.i.lmodev -textvar ::mode -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .m.i.lmoder -text "}" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	pack [label .m.i.mr -text ">" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side right 
	pack [label .m.i.lme -textvar ::me -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side right
	pack [label .m.i.ml -text "<" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side right
	pack [label .m.i.lmn -textvar ::menick -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side right 

	set cmd_s [concat {+ click_mailwindow s .m.f.t %x %y}]
	set cmd_f [concat {+ click_mailwindow f .m.f.t %x %y}]
	set cmd_h [concat {+ click_mailwindow h .m.f.t %x %y}]
	set cmd_a [concat {+ click_mailwindow a .m.f.t %x %y}]
	.m.f.t tag bind searchlink <1> $cmd_s
	.m.f.t tag bind mlfilelink <1> $cmd_f
	.m.f.t tag bind headerlink <1> $cmd_h
	.m.f.t tag bind attachlink <1> $cmd_a
	bind .m.b.menu <1> {+ tk_popup .nmenu %X %Y}
	
	bind .m.x.ls <Key-Return> $read_cmd
}

proc mail_header_to_contact {header} {
	set h [header_to_dict $header]
	dict set c peerid [dict get $h from]
	dict set c pubkey [dict get $h kfrom]
	dict set c nickname [dict get $h nickname]
	dict set c birthday {}
	dict set c sex {}
	dict set c country {}
	dict set c city {}
	dict set c epoch [clock seconds] 
	dict set c sig {}
	set contact [dict_to_contact $c]
	return $contact
}

proc show_group_selection {} {
	if { [winfo exists .sgs] == 1} {
		puts "ERR window exists"
		return
	}
	set ::mode {g}
	set choose_cmd {
		set grp [lindex $::jgroups_k [lindex [.sgs.f.l index active] 0]]
		if { $grp != "" } {
			set ::ml_cur_group $::jgroups([lindex $::jgroups_k [lindex [.sgs.f.l index active] 0]])
			set ::ml_cur_group_l [string range [dict get [ml_groupdict $::ml_cur_group] name] 0 12]
			ml_showlist [dict get [ml_groupdict $::ml_cur_group] gid]
		}
		destroy .sgs
	}
	toplevel .sgs
	wm title .sgs "Choose group"
	pack [panedwindow .sgs.p -ori vert] -fill both -expand 1
	.sgs.p add [frame .sgs.b]
	pack [button .sgs.b.n -text "create" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_groupform ; destroy .sgs}] -fill both -side left 
	pack [button .sgs.b.c -text "choose" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command $choose_cmd ] -fill both -side right
	pack [button .sgs.b.d -text "detail" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {catch { show_group_details $::jgroups([lindex $::jgroups_k [lindex [.sgs.f.l index active] 0]])}} ] -fill both -side right
	pack [button .sgs.b.a -text "dir" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {destroy .sgs ; show_group_directory} ] -fill both -side right
	pack [button .sgs.b.x -text "cancel" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {destroy .sgs} ] -fill both -side left
	.sgs.p add [frame .sgs.f]
	pack [scrollbar .sgs.f.y -activebackground {#606060}  -troughcolor {#606060}  -command ".sgs.f.l yview"] -fill y -side right
	pack [listbox .sgs.f.l -listvariable ::jgroups_m -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont -width 60 -height 12 -yscrollc ".sgs.f.y set"] -fill both -expand 1 -side right
	#update_groups
	set ::jgroups_k {}
	set ::jgroups_m {}
	foreach {k v} [array get ::jgroups] {
		set b [ml_groupdict $v]
		if { $b == "" } {
			continue	
		}
		set bm "[clock format [dict get $b epoch] -format {%Y-%d-%m %H:%M:%S}] | [string range [dict get $b gid] 0 3] | [dict get $b name]"
		lappend ::jgroups_k	$k
		lappend ::jgroups_m $bm 
	}	
	bind .sgs.f.l <Key-Return> $choose_cmd
}

proc show_contact_selection {} {
	if { [winfo exists .scs] == 1} {
		puts "ERR window exists"
		return
	}
	set ::mode {p}
	set choose_cmd {
		set ::ml_cur_person $::buddies([lindex $::buddies_k [lindex [.sbs.f.l index active] 0]]) ;
		set ::ml_cur_person_l [dict get [contact_to_dict $::ml_cur_person] nickname] ;
		if {$::ml_cur_person != ""} {
			ml_showpersonlist [dict get [contact_to_dict $::ml_cur_person] peerid]
		}
		destroy .sbs
	}
	set detail_cmd {
		set hash [lindex $::buddies_k [lindex [.sbs.f.l index active] 0]] ;
		show_buddy_details $hash
	}
	toplevel .sbs
	wm title .sbs "Choose person"
	pack [panedwindow .sbs.p -ori vert] -fill both -expand 1
	.sbs.p add [frame .sbs.b]
	#pack [button .sbs.b.n -text "create" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_contactform ; destroy .scs}] -fill both -side left 
	pack [button .sbs.b.c -text "choose" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command $choose_cmd ] -fill both -side right
	pack [button .sbs.b.d -text "detail" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command $detail_cmd ] -fill both -side right
	pack [button .sbs.b.a -text "dir" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {destroy .sbs ; show_directory} ] -fill both -side right
	pack [button .sbs.b.x -text "cancel" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {destroy .sbs} ] -fill both -side left
	.sbs.p add [frame .sbs.f]
	pack [scrollbar .sbs.f.y -activebackground {#606060}  -troughcolor {#606060}  -command ".sbs.f.l yview"] -fill y -side right
	pack [listbox .sbs.f.l -listvariable ::buddies_m -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont -width 60 -height 12 -yscrollc ".sbs.f.y set"] -fill both -expand 1 -side right
	update_buddies
	set ::buddies_k {}
	set ::buddies_m {}
	foreach {k v} [array get ::buddies] {
		set b [contact_to_dict $v]
		if { $b == "" } {
			continue	
		}
		set bm "[clock format [dict get $b epoch] -format {%Y-%d-%m %H:%M:%S}] | [string range [dict get $b peerid] 0 3] | [dict get $b nickname]"
		lappend ::buddies_k	$k
		lappend ::buddies_m $bm 
	}	
	bind .sbs.f.l <Key-Return> $choose_cmd
}

proc show_editor {contact} {
	if { $::mode == "g" && [llength $::ml_cur_group] == 0 } {
		show_group_selection
	}
	if { $::mode == "p" && [llength $::ml_cur_person] == 0 } {
		show_contact_selection
	}
	if { $::mode == "m" && [llength $contact] == 0} {
		puts "ERR no contact : got $contact"
		show_contact_selection
	}
	
	if { [winfo exists .e] == 1} {
		puts "ERR window exists"
		return
	}

	if { $::mode == "m" } {
		set ::e_contact $contact
	} elseif { $::mode == "p" } {
		set ::e_contact $::ml_cur_person
	} else {
		set ::e_contact {}
	}

	set last [string last "Re:Re:" $::e_subject ]
	if { $last > 0 } {
		set ::e_subject [string range $::e_subject $last end]
	}

	toplevel .e
	wm title .e "Compose"
	pack [panedwindow .e.p -ori vert] -fill both -expand 1
	.e.p add [frame .e.b0]
	pack [label .e.b0.ml -text "From: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -justify right] -fill both -side left 
	pack [label .e.b0.m -text "$::menick <$::me>" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -justify left] -fill both -side left 
	if { $::mode == {p} } {
		pack [button .e.b0.v -text "commit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {ml_add_personhdrs [dict get [contact_to_dict $::ml_cur_person] peerid] [form_message] ; destroy .e } ] -fill both -side right
	} elseif { $::mode == {g} } {
		pack [button .e.b0.v -text "commit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {ml_add_hdrs [dict get [ml_groupdict $::ml_cur_group] gid] [form_message] ; destroy .e } ] -fill both -side right
	} else {
		#pack [button .e.b0.v -text "commit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {sc_out [form_message ] ; destroy .e } ] -fill both -side right
		pack [button .e.b0.v -text "commit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {form_message ; destroy .e } ] -fill both -side right
	}
	#pack [button .e.b0.at -text "attach" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "attach_file"] -fill both -side right 
	pack [button .e.b0.i -text "insert" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "insert_single_file e {}"] -fill both -side right 
	pack [button .e.b0.a -text "add file link" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_mlfilechooser} ] -fill both -side right
	#pack [button .e.b0.x -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {destroy .e}] -fill both -side right 
	.e.p add [frame .e.b1]
	pack [label .e.b1.tl -text "To: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -justify right] -fill both -side left 
	if { $::mode == "m" && $::e_contact != "" } {
		set ::e_to [dict get [contact_to_dict $::e_contact] nickname]
		pack [label .e.b1.t -text $::e_to -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 80 ] -fill both -expand 1 -side left 
	} elseif { $::mode == "p" && $::e_contact != "" } {
		set ::e_to [dict get [contact_to_dict $::e_contact] nickname]
		pack [label .e.b1.t -text $::e_to -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 80 ] -fill both -expand 1 -side left
	} elseif { $::mode == "g" } {
		set ::e_to [dict get [ml_groupdict $::ml_cur_group] name]
		pack [label .e.b1.t -text $::e_to -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 80 ] -fill both -expand 1 -side left
	} else {
		set ::mode {n}
		pack [entry .e.b1.t -textvariable ::e_to -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont -width 80 ] -fill both -expand 1 -side left 
	}
	.e.p add [frame .e.b2]
	pack [label .e.b2.sl -text "Subject: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -justify right] -fill both -side left
	pack [entry .e.b2.s -textvariable ::e_subject -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::listfont -width 80] -fill both -expand 1 -side left
	if { $::e_parent != "" } {
		.e.p add [frame .e.b3]
		pack [label .e.b3.sl -text "Parent: " -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -justify right] -fill both -side left
		pack [label .e.b3.s -textvariable ::e_parent -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 80] -fill both -expand 1 -side left 
	}
	.e.p add [frame .e.x]
	pack [scrollbar .e.x.y -activebackground {#606060}  -troughcolor {#606060}  -command ".e.x.t yview"] -fill y -side right
	pack [text .e.x.t -wrap word -yscrollc ".e.x.y set" \
		-selectforeground {#6090c0} -selectbackground {#606060} \
		-padx 5 -pady 3 -height 12 -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -font $::listfont] \
		-fill both -expand 1 -side right
	.e.x.t tag configure red -foreground {#c06060} 
	.e.x.t tag configure cyan -foreground {#60c0c0}
	.e.x.t tag configure blue -foreground {#6060c0}
	.e.x.t tag configure yellow -foreground {#c0c060}
	.e.x.t tag configure magenta -foreground {#c060c0}
	.e.x.t tag configure hide -elide true
	.e.x.t tag configure mlfilelink -foreground {#6060c0} -underline true
	.e.x.t tag configure attachlink -foreground {#c06060} -underline true
	.e.x.t tag configure headerlink -foreground {#6060c0} -underline true
	.e.x.t tag configure searchlink -foreground {#6060c0} -underline true
	if { $::e_parent != "" && $::mode == {g} } {
		set ph [header_to_dict $::e_parent]
		if { $ph == {} } {
			break
		} 
		set body [encoding convertfrom utf-8 [ml_get_eml [dict get $ph hash]]]
		set bodylines [split $body "\n"]
		set from [lindex $bodylines 0]
		set first [string first ":" $from]
		set from [string trim [string range $from $first+1 end]]
		#set to [lindex $bodylines 1]
		#set first [string first ":" $to]
		#set to [string trim [string range $to $first+1 end]]
		.e.x.t insert end "$from wrote:\n" {magenta}
		set lines [lrange $bodylines 5 end]
		foreach line $lines {
			if { [lindex $line 0] == "HDR" || [lindex $line 0] == "FILE" || [lindex $line 0] == "ATTACH" } {
				set line "[string tolower [lindex $line 0]]://<...>"
			}
			.e.x.t insert end "> $line\n" {magenta}
		}
		.e.x.t yview end
	}
}

proc load_id {} {
	set path [file join $::filepath "key"]
	if {[ file exists $path ] == 0} {
		set ::mekey [::pki::rsa::generate 1024]
		set ::me [::sha1::sha1 [::pki::public_key $::mekey]]
	} else {
		set fchan [open $path r]
		fconfigure $fchan -translation binary
		set data [read $fchan]
		set ::mekey [::pki::pkcs::parse_key $data]
		set ::me [::sha1::sha1 [::pki::public_key $::mekey]]
		close $fchan
	}	

	set path [file join $::filepath "nickname"]
	if {[ file exists $path ] == 0} {
		set r [expr "[clock microseconds]%8"]
		set ns [list "Ethereal Sapphire" "Weird Vulture" "Water Dragon" "Sunshine Emerald" "Stratagus" "Domesticus" "Sacellarius" "Just a guy"]
		set ::menick [lindex $ns $r]
	} else {
		set fchan [open $path r]
		fconfigure $fchan -translation binary
		gets $fchan ::menick
		close $fchan
	}
}

proc load_port {} {
	set path [file join $::filepath "port"]
	if {[ file exists $path ] == 0} {
		set ::myport [expr "1234+([clock microseconds])%50000"]
	} else {
		set fchan [open $path r]
		fconfigure $fchan -translation binary
		gets $fchan ::myport
		close $fchan	
	}
}

proc write_id {} {
	set path [file join $::filepath "key"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	puts -nonewline $fchan [::pki::key $::mekey]
	close $fchan
	set path [file join $::filepath "nickname"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	puts -nonewline $fchan $::menick
	close $fchan
}

proc write_port {} {
	set path [file join $::filepath "port"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	puts -nonewline $fchan $::myport
	close $fchan
}

proc load_contact {} {
	set path [file join $::filepath "contact"]
	if {[ file exists $path ] == 0} {
		set ::mycontact [form_contact]
	} else {
		set fchan [open $path r]
		fconfigure $fchan -translation binary
		gets $fchan ::mycontact
		close $fchan
	}
}

proc write_contact {} {
	set path [file join $::filepath "contact"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	puts -nonewline $fchan $::mycontact
	close $fchan
}

proc load_peers {} {
	set path [file join $::filepath "nodes"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		if {$line == "" } {
			puts "ERR peers empty line"
			continue
		} 
		puts "load_peers line $line"
		set pkey [lindex $line 0]
		set peer [lindex $line 1]
		set host [lindex [split $peer {:}] 1]
		set port [lindex [split $peer {:}] 2]
		set key [lindex [split $peer {:}] 0]
		if { $peer == "" || $host == "" || $port == "" || $key == "" } {
			puts "ERR load empty peer $peer"
			continue
		}
		array set ::peerstore [list $pkey $peer]
	}
	close $fchan
}

proc load_buckets {} {
	set path [file join $::filepath "buckets"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lindex [split $line { }] 1]
		array set ::b [list $pkey $pval]
	}
	close $fchan
}

proc load_values {} {
	set path [file join $::filepath "values"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lrange [split $line { }] 1 end]
		if { [expr "[clock microseconds]%7"] != 0 } {
			array set ::valuestore [list $pkey $pval]
		}
	}
	close $fchan
}

proc load_contacts {} {
	set path [file join $::filepath "contacts"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lrange [split $line { }] 1 end]
		puts "pkey $pkey pval $pval"
		array set ::contacts [list $pkey $pval]
	}
	close $fchan
}

proc load_headers {} {
	set path [file join $::filepath "headers"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lindex [split $line { }] 1]
		array set ::headers [list $pkey $pval]
	}
	close $fchan
}

proc load_groups {} {
	set path [file join $::filepath "groups"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lindex [split $line { }] 1]
		array set ::groups [list $pkey $pval]
	}
	close $fchan
	set path [file join $::filepath "my_groups"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lindex [split $line { }] 1]
		array set ::my_groups [list $pkey $pval]
	}
	close $fchan
}

proc load_jgroups {} {
	set path [file join $::filepath "jgroups"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lrange [split $line { }] 1 end]
		array set ::jgroups [list $pkey $pval]
		after 5000 [list ml_add_srcs $pkey $::me]
		array set ::sources [list $pkey $::me]
		after 5000 [list sc_publishgroup $pval]
		ml_add_sigreq g $pkey $::mycontact
	}
	close $fchan
}

proc load_sources {} {
	set path [file join $::filepath "sources"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lindex [split $line { }] 1]
		array set ::sources [list $pkey $pval]
	}
	close $fchan
}

proc load_buddies {} {
	set path [file join $::filepath "buddies"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lindex [split $line { }] 1]
		puts "load buddy $pkey $pval"
		array set ::buddies [list $pkey $pval]
		array set ::contacts [list $pkey $pval]
	}
	close $fchan
}

proc load_files {} {
	set path [file join $::filepath "file_by_hash"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lrange [split $line { }] 1 end]
		puts "load file_by_hash $pkey $pval"
		array set ::file_by_hash [list $pkey $pval]
	}
	close $fchan

	set path [file join $::filepath "hash_by_file"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lrange [split $line { }] 1 end]
		puts "load hash_by_file $pkey $pval"
		array set ::hash_by_file [list $pkey $pval]
	}
	close $fchan
}

proc load_dls {} {
	set path [file join $::filepath "dl_by_hash"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lrange [split $line { }] 1 end]
		puts "load dl_by_hash $pkey $pval"
		array set ::dl_by_hash [list $pkey $pval]
	}
	close $fchan

	set path [file join $::filepath "dlstate_by_hash"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lrange [split $line { }] 1 end]
		puts "load dlstate_by_hash $pkey $pval"
		array set ::dlstate_by_hash [list $pkey $pval]
	}
	close $fchan
}

proc load_keys {} {
	set path [file join $::filepath "keys"]
	if {[ file exists $path ] == 0} {
		return
	}
	set line {}
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	while {[gets $fchan line] >= 0} {
		set pkey [lindex [split $line { }] 0]
		set pval [lrange [split $line { }] 1 end]
		puts "load keys $pkey $pval"
		array set ::keys [list $pkey $pval]
	}
	close $fchan
}

proc write_peers {} {
	set path [file join $::filepath "nodes"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {name peer} [array get ::peerstore] {
		if { $peer == "" } {
			puts "ERR write empty peer"
			continue
		}
		if {[regexp -all {:} $peer] != 2} {
			puts "ERR malformed peer"	
			continue
		}
		puts $fchan "$name $peer"
	}
	close $fchan
}

proc write_buckets {} {
	set path [file join $::filepath "buckets"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::b] {
		puts $fchan "$key $val"
	}
	close $fchan
}

proc write_values {} {
	set path [file join $::filepath "values"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::valuestore] {
		puts $fchan "$key $val"
	}
}

proc write_contacts {} {
	set path [file join $::filepath "contacts"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::contacts] {
		puts $fchan "$key $val"
	}
}

proc write_headers {} {
	set path [file join $::filepath "headers"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::headers] {
		puts $fchan "$key $val"
	}
	close $fchan
}

proc write_groups {} {
	set path [file join $::filepath "groups"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::groups] {
		puts $fchan "$key $val"
	}
	close $fchan
	set path [file join $::filepath "my_groups"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::my_groups] {
		puts $fchan "$key $val"
	}
	close $fchan
}

proc write_jgroups {} {
	set path [file join $::filepath "jgroups"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::jgroups] {
		puts $fchan "$key $val"
	}
	close $fchan
}

proc write_sources {} {
	set path [file join $::filepath "sources"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::sources] {
		puts $fchan "$key $val"
	}
	close $fchan
}

proc write_buddies {} {
	set path [file join $::filepath "buddies"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::buddies] {
		puts $fchan "$key $val"
	}
	close $fchan
}

proc write_files {} {
	set path [file join $::filepath "file_by_hash"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::file_by_hash] {	
		puts $fchan "$key $val"
	}
	close $fchan

	set path [file join $::filepath "hash_by_file"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::hash_by_file] {	
		puts $fchan "$key $val"
	}
	close $fchan
}

proc write_dls {} {
	set path [file join $::filepath "dl_by_hash"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::dl_by_hash] {	
		if { $val == {} } {
			continue
		}
		puts $fchan "$key $val"
	}
	close $fchan

	set path [file join $::filepath "dlstate_by_hash"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::dlstate_by_hash] {	
		if { $val == {} } {
			continue
		}
		puts $fchan "$key $val"
	}
	close $fchan
}

proc write_keys {} {
	set path [file join $::filepath "keys"]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	foreach {key val} [array get ::keys] {	
		puts $fchan "$key $val"
	}
	close $fchan
}

proc write_sig {} {
	foreach kind [list person_to_sig person_to_sigreq group_to_sig group_to_sigreq] {
		set path [file join $::filepath "$kind"]
		set fchan [open $path w]
		fconfigure $fchan -translation binary
		foreach {key val} [array get "::$kind"] {	
			puts $fchan "$key $val"
		}
		close $fchan
	}
}

proc load_sig {} {
	foreach kind [list person_to_sig person_to_sigreq group_to_sig group_to_sigreq] {
		set path [file join $::filepath "$kind"]
		if { [file exists $path] == 0 } {
			continue
		}
		set fchan [open $path r]
		fconfigure $fchan -translation binary
		while {[gets $fchan line] >= 0} {
			set pkey [lindex [split $line { }] 0]
			set pval [lrange [split $line { }] 1 end]
			puts "load keys $pkey $pval"
			array set "::$kind" [list $pkey $pval]
		}
		close $fchan
	}
	foreach {key val} [array get ::my_groups] {
		if { [array get ::group_to_sigreq "$key,$::me"] == "" } {
			set req [lindex [array get ::group_to_sigreq "$key,$::me"] end]
			ml_add_sigreq g $key $::mycontact 
			set pkey [::pki::pkcs::parse_key [unwrap $val]]
			set sig "$req:[wrap [::pki::sign $req $pkey]]"
			array set ::group_to_sig [list "$key,$::me" $sig]
		}
	}
}

proc form_message {} {
	
	#set kfrom "[::pki::public_key $::mekey]"
	set kfrom {} 
	set kto {} 
	set hfrom $::me
	set from "[string range $::menick 0 64] <$::me>"
	set subject "[string range $::e_subject 0 64]"
	set epoch [clock seconds]

	if { $::e_contact != "" && $::mode == "m" } {
		set c [contact_to_dict $::e_contact]
		set pubkey [::pki::pkcs::parse_public_key [dict get $c pubkey]]
		puts "forming message to contact $c"
		set group {}
		set hto "[dict get $c peerid]"
		set to "[string range [dict get $c nickname] 0 64] <[dict get $c peerid]>"
		set hsubject "hidden"
		set type "m"
	} elseif { $::ml_cur_person != "" && $::mode == "p" } {
		set p [contact_to_dict $::ml_cur_person]
		set pubkey [::pki::pkcs::parse_public_key [dict get $p pubkey]]
		puts "forming message to person $p"
		set group {} 
		set hfrom $::me
		set hto [dict get $p peerid]
		set to "[string range [dict get $p nickname] 0 64] <[dict get $p peerid]>" 
		set hsubject "[string range $::e_subject 0 64]"
		set type "p"
	} elseif { $::ml_cur_group != "" && $::mode == "g" } {
		set g [ml_groupdict $::ml_cur_group]
		puts "forming message to group $g"
		set group [dict get $g gid] 
		set hfrom $::me
		set hto [dict get $g gid]
		set to "[string range [dict get $g name] 0 64] <[dict get $g gid]>" 
		set hsubject "[string range $::e_subject 0 64]"
		set type "g"
	} elseif { $::mode == "n" } {
		puts "forming message to news"
		set group {}
		set hto [string range $::e_to 0 64]
		set to [string range $::e_to 0 64]
		set hsubject "[string range $::e_subject 0 64]"
		set type "n"
	} else {
		return
	}
	
	#set body [string range [.e.x.t get 1.0 end] 0 65536]
	set body [.e.x.t get 1.0 end]

	set whole {}
	append whole "From:\t$from\n"
	append whole "To:\t$to\n"
	append whole "Subject:\t$subject\n"
	append whole "Epoch:\t$epoch\n"
	append whole "\n"
	if { $::e_parent != "" } {
		append whole "HDR [dict get [header_to_dict $::e_parent] hash] Parent\n"
	}
	append whole "$body\n\n"

	puts "Formed message:"
	puts $whole

	set whole [encoding convertto utf-8 $whole]	

	if { ($::e_contact != "" && $::mode == "m") || ($::ml_cur_person != "" && $::mode == "p") } {
		puts "using pubkey to encrypt:"
		puts $pubkey
		set pwhole $whole
		set aeskey [read $::randomchan 32]
		puts "encrypting aeskey [wrap $aeskey]"
		set aeskey_e [::pki::encrypt -hex -pub $aeskey $pubkey]
		set nil_block [string repeat \0 16]
		set whole_e [::aes::aes -mode cbc -dir encrypt -key $aeskey -iv $nil_block $whole] 
		puts "aes-encrypted message $whole_e"
		set whole [wrap $aeskey_e],[wrap $whole_e]
	}

	set len [string length $whole]
	set hash [::sha1::sha1 $whole]

	set h {}
	dict set h hash $hash 
	dict set h group $group
	dict set h len $len
	dict set h epoch $epoch
	dict set h from $hfrom
	dict set h to $hto 
	dict set h subject $hsubject
	dict set h type $type
	dict set h nickname $::menick
	dict set h kfrom $kfrom
	dict set h kto $kto 
	dict set h gsig {} 
	set header [dict_to_header $h] 
	puts "Formed header:"
	puts $header

	set path [file join $::filepath "mailnews" $hash]
	set fchan [open $path w]
	fconfigure $fchan -translation binary
	puts -nonewline $fchan $whole	
	flush $fchan
	close $fchan
	
	if { ($::e_contact != "" && $::mode == "m") || ($::ml_cur_person != "" && $::mode == "p") } {
		set path [file join $::filepath mailnews plain $hash]
		set fchan [open $path w]
		fconfigure $fchan -translation binary
		puts -nonewline $fchan $pwhole	
		flush $fchan
		close $fchan
	}

	if { $::mode == "g" } {
		set gsrc [ml_get_srcs $hto]
		if { [lindex $gsrc 1] == 0 } {
			break
		}
		set peers [lrange $gsrc 2 end]
		foreach peer $peers {
			set speer [split $peer {:}]
			if { [lindex $speer 0] == $::me } {
				continue
			}
			ml_genc [lindex $speer 0] [lindex $speer 1] [lindex $speer 2] "MAIL 0 DIG [list $hto 1 $header]" 0
		}
		after 500 [list ml_showlist $hto]
	} elseif { $::mode == "p" } {
		ml_genc $hto {} {} "MAIL 0 PWT [list $::me 1 $header]" 0
		after 500 [list ml_showpersonlist $hto]
	}

	return $header
}

proc form_contact {} {
	puts "form_contact"
	#			peerid:b64(pubkey):b64(nickname):b64(age):b64(sex(M/F/O/N)):b64(country):b64(city):epoch:b64(sig - sign previous with pubkey):... probable authority pkey&sig
	set c $::me
	append c ":[wrap [::pki::public_key $::mekey]]"
	append c ":[wrap [string range $::menick 0 64]]"
	append c ":[wrap [string range $::c_bday 0 16]]"
	append c ":[wrap [string range $::c_sex 0 16]]"
	append c ":[wrap [string range $::c_country 0 16]]"
	append c ":[wrap [string range $::c_city 0 16]]"
	append c ":[clock seconds]"
	append c ":[wrap [::pki::sign $c $::mekey]]"
	puts "contact is $c"
	set ::mycontact $c
	return $c
}

proc contact_to_dict {contact} {
	#puts "contact_to_dict"
	if {[regexp -all {:} $contact] != 8} {
		return
	}
	dict set d peerid "[lindex [split $contact {:}] 0]" 
	dict set d pubkey "[unwrap [lindex [split $contact {:}] 1]]"
	if { [dict get $d peerid] != [::sha1::sha1 [dict get $d pubkey]] } {
		return
	}
	dict set d nickname "[unwrap [lindex [split $contact {:}] 2]]"
	dict set d birthday "[unwrap [lindex [split $contact {:}] 3]]"
	dict set d sex "[unwrap [lindex [split $contact {:}] 4]]"
	dict set d country "[unwrap [lindex [split $contact {:}] 5]]"
	dict set d city "[unwrap [lindex [split $contact {:}] 6]]"
	dict set d epoch "[lindex [split $contact {:}] 7]"
	dict set d sig "[unwrap [lindex [split $contact {:}] 8]]"
	return $d
}

proc dict_to_contact {d} {
	#puts "dict_to_contact"
	set c {}
	append c [dict get $d peerid]
	append c :
	append c [wrap [dict get $d pubkey]]
	append c :
	append c [wrap [dict get $d nickname]]
	append c :
	append c [wrap [dict get $d birthday]]
	append c :
	append c [wrap [dict get $d sex]]
	append c :
	append c [wrap [dict get $d country]]
	append c :
	append c [wrap [dict get $d city]]
	append c :
	append c [dict get $d epoch]
	append c :
	append c [wrap [dict get $d sig]]
	return $c
}

proc update_mail {force} {
	if { [winfo exists .m] == 0 } {
		return
	}
	#if { ($::search == "") && ($::text == "") } {
	#	return
	#}
	puts "update mail"
	#if { $::search != $::oldsearch } {
	#	read_headers $::search
	#	set ::oldsearch $::search
	#}
	#if { $::text != $::oldtext } {
	#	read_message [lindex [split $::text {,}] 0]
	#	set ::oldtext $::text
	#}
	if { $::search != "" || $force == "force" } {
		read_headers $::search
		#after 5000 [list update_mail {}]
	}
	if { $::text != "" || $force == "force" } {
		read_message [lindex [split $::text {,}] 0]
	}
}

proc update_directory {} {
	if { [winfo exists .d] == 0 } {
		return
	}
	#if { ($::contactsearch == "") } {
	#	return
	#}
	puts "update directory"
	#if { $::contactsearch != $::oldcontactsearch } {
	#	read_contacts $::contactsearch
	#	set ::oldcontactsearch $::contactsearch
	#}
	if { ($::contactsearch != "") } {
		read_contacts $::contactsearch
	}
	after 3000 update_directory
}

proc update_group_directory {} {
	if { [winfo exists .gd] == 0 } {
		return
	}
	puts "update group directory"
	if { ($::groupsearch != "") } {
		read_groups $::groupsearch
	}
	after 3000 update_group_directory
	
}

proc read_message {header} {

	if { $header == "" } {
		puts "empty header, return"
		return
	}

	set h [header_to_dict $header]
	if { $h == "" } {
		return
	}
	
	set hash [dict get $h hash]

	puts "Reading message for hash $hash"
	if { $hash == "" } {
		puts "empty hash, return"
		return
	}

	.m.f.t delete 1.0 end

	set path [file join $::filepath "mailnews" $hash]
	puts "file exists $path == [file exists $path]"
	if { [file exists $path] == 0 } {
		.m.f.t insert end "no data yet"
		return
	}

	puts "Opening message by $path"
	set fchan [open $path r]
	fconfigure $fchan -translation binary
	set whole [read $fchan]
	close $fchan

	set type [dict get $h type]
	set comment {}
	if { ($type == "m" && [dict get $h to] == $::me) || ($type == "p" && [dict get $h to] == $::me) } {
		puts "message is personal, to me"
		set comment "personal to me"
		set aeskey_e [unwrap [lindex [split $whole {,}] 0]]
		puts "aeskey_e is $aeskey_e"
		set aeskey [::pki::decrypt -hex -priv $aeskey_e $::mekey]
		puts "decrypted aeskey [wrap -maxlen 0 $aeskey]"
		set whole_e [unwrap [lindex [split $whole {,}] 1]]
		puts "aes-encrypted message $whole_e"
		set nil_block [string repeat \0 16]
		set whole [string trimright [::aes::aes -mode cbc -dir decrypt -key $aeskey -iv $nil_block $whole_e] \0]
		puts "whole is $whole"
		set fchan [open [file join $::filepath mailnews dec $hash] w]
		fconfigure $fchan -translation binary
		puts -nonewline $fchan $whole
		close $fchan
	} elseif { $type == "m" && ([dict get $h kfrom] == [::pki::public_key $::mekey] || [dict get $h kfrom] == {}) && [file exists [file join $::filepath mailnews plain $hash]] == 1} {
		puts "message is personal, by me"
		set comment "personal by me"
		puts "Opening message by [file join $::filepath mailnews plain $hash]"
		set fchan [open [file join $::filepath mailnews plain $hash] r]
		fconfigure $fchan -translation binary
		set whole [read $fchan]
		close $fchan
	} elseif { $type == "m" } {
		puts "message is personal, not by me or for me"
		return
	} elseif { $type == "n" } {
		puts "message is news"
		set comment "news article"
	} else {
		puts "something is off, return, header: $h"
		return
	}

	set lines [split $whole "\n"]
	set from [lindex $lines 0]
	set to [lindex $lines 1]
	set subject [lindex $lines 2]
	set epoch [lindex $lines 3]
	set bodylines [lrange $lines 5 end]
	puts "Header is:"
	puts "$header"
	puts "Message read from filesystem is:"
	puts "$whole"
	.m.f.t tag configure red -foreground {#c06060} 
	.m.f.t tag configure cyan -foreground {#60c0c0}
	.m.f.t tag configure blue -foreground {#6060c0}
	.m.f.t tag configure yellow -foreground {#c0c060}
	.m.f.t tag configure headerlink -foreground {#6060c0} -underline true
	.m.f.t tag configure searchlink -foreground {#6060c0} -underline true
	.m.f.t tag configure mlfilelink -foreground {#6060c0} -underline true
	.m.f.t tag configure attachlink -foreground {#c06060} -underline true
	.m.f.t insert end "   Sent: [clock format [dict get $h epoch] -format {%Y-%m-%d %H:%M:%S}]\n" {red}
	.m.f.t insert end "Comment: $comment\n" {red}
	.m.f.t insert end "   $from\n" {cyan}
	.m.f.t insert end "     $to\n" {cyan}
	.m.f.t insert end "$subject\n" {searchlink}
	.m.f.t insert end "  Epoch: [clock format [lindex [split $epoch {: }] end] -format {%Y-%m-%d %H:%M:%S}]\n" {red}
	.m.f.t insert end "\n"
	foreach line $bodylines {
		if { [lindex [split $line { }] 0] == "FILE" } {
			set tag {blue mlfilelink}
		} elseif { [string index $line 0] == ">" } {
			set tage {magenta}
		} else {
			set tag {} 
		}
		.m.f.t insert end "$line\n" $tag
	}
	.m.f.t insert end "\n" $tag
	return
}

proc click_mailwindow {t w x y} {
	if { $t == {s} } {
		set range [$w tag prevrange searchlink [$w index @$x,$y]]
		set searchlink [eval $w get $range]
		if { $searchlink != "" } {
			#show_headers [sc_get_headers [prep_header_keys [lrange [split $searchlink {: }] 1 end]]]
			set ::searchfield [lrange [split $searchlink {: }] 1 end]
		}
	} elseif { $t == {f} } {
		set range [$w tag prevrange mlfilelink [$w index @$x,$y]]
		set mlfilelink [eval $w get $range]
		if { $mlfilelink != "" } {
			set mlfile_header [lindex [split $mlfilelink {<>}] 1]
			puts "mlfile_header $mlfile_header"
			#set mlh [header_to_dict $mlfile_header]
			set mlh [dl_reqdict $mlfile_header]
			puts "mlh $mlh"
			#set name [dict get $mlh subject]
			set name [dict get $mlh filename]
			puts "name $name"
			#set hash [dict get $mlh hash]
			set hash [dict get $mlh filehash]
			#sc_download $mlfile_header
			#dl_add $mlfile_header {} {}
			show_filedialog $mlfile_header {}
		}
	} elseif { $t == {h} } {
		set range [$w tag prevrange headerlink [$w index @$x,$y]]	
		set headerlink [eval $w get $range]
		set hdrshrt [string trim [lindex $headerlink 1]]
		if { $headerlink != "" } {
			if {[regexp -all {:} $hdrshrt] > 0} {
				set ::ml_cur_header $hdrshrt
			} else {
				if { $::mode == {p} && $::ml_cur_person != "" } {
					set hdrs [ml_get_personhdrs [dict get [contact_to_dict $::ml_cur_person] peerid]]
				} elseif { $::mode == {g} && $::ml_cur_group != "" } {
					set hdrs [ml_get_hdrs [dict get [ml_groupdict $::ml_cur_group] gid]]
				}
				set hdrs [lsort -unique [lsearch -all -inline $hdrs "$hdrshrt:*"]]
				foreach hdr $hdrs {
					if {[regexp -all {:} $hdr] > 0} {
						set ::ml_cur_header $hdr
						break
					}
				}
			}
			ml_showmsg $::ml_cur_header
		}
	} elseif { $t == {a} } {
		set range [$w tag prevrange attachlink [$w index @$x,$y]]
		set attachlink [eval $w get $range]
		if { $attachlink == "" } {
			return
		}
		set attach_desc [string trim [string map {{ATTACH } {}} [lindex [split $attachlink {()}] 0]]]
		set attach_data [lindex [split $attachlink {<>}] 1]
		save_file $attach_desc $attach_data
	}
}

proc save_file {desc data} {
	set fn [tk_getSaveFile -initialfile $desc]
	if { $fn == "" || [file exists $fn] } {
		return
	}
	set f [open $fn w]
	fconfigure $f -translation binary -encoding binary
	puts -nonewline $f [unwrap $data]
	flush $f
	close $f	
}

proc show_mlfilechooser {} {
	if {[winfo exists .mfo] == 1} {
		return
	}
	toplevel .mfo
	wm title .mfo "Choose file to link"
	pack [panedwindow .mfo.p -ori vert] -fill both -expand 1
	.mfo.p add [frame .mfo.t] -stretch never
	pack [label .mfo.t.l -text "Choose file to link :" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	.mfo.p add [frame .mfo.ll] -stretch never
	pack [label .mfo.ll.name -text "name" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -expand 1 -side left 
	pack [label .mfo.ll.size -text "size" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 8 ] -fill y -side left
	pack [label .mfo.ll.hash -text "hash" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font -width 12 ] -fill y -side left
	.mfo.p add [frame .mfo.l] -stretch always
	pack [listbox .mfo.l.l -listvariable ::mfo_l -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -yscrollc ".mfo.l.y set"] -fill both -expand 1 -side left
	pack [scrollbar .mfo.l.y -activebackground {#606060}  -troughcolor {#606060}  -command {tl_yview 1 .mfo.l.l}] -fill y -side right
	.mfo.p add [frame .mfo.b] -stretch never
	pack [button .mfo.b.x -text "cancel" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "destroy .mfo"] -fill both -side right
	pack [button .mfo.b.s -text "insert" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "insert_mlfilelink; destroy .mfo"] -fill both -side right

	set ::mfo_l {}
	set ::mfo_h {}
	foreach {hash details} [array get ::file_by_hash] {
		array set ::sources [list $hash $::me]
		set name [lindex [file split [unwrap [dict get $details name]]] end]
		set size [dict get $details size]
		lappend ::mfo_l "[string range $hash 0 3] $name ($size)"
		#set h {}
		#dict set h hash $hash
		#dict set h epoch [clock seconds]
		#dict set h group {}
		#dict set h len $size
		#dict set h from {}
		#dict set h to {}
		#dict set h nickname {} 
		#dict set h subject [lindex [file split $name] end]
		#dict set h type {f}
		#dict set h kfrom {}
		#dict set h kto {}
		#dict set h gsig {}
		#lappend ::mfo_h [dict_to_header $h]
		dict set r filehash $hash
		dict set r filename $name
		dict set r offset 0
		dict set r len $size
		lappend ::mfo_h [dl_dictreq $r]
	}

	proc insert_mlfilelink {} {
		set link [lindex $::mfo_l [lindex [.mfo.l.l index active] 0]]
		set linktext "FILE [lindex $::mfo_l [lindex [.mfo.l.l index active] 0]] <[lindex $::mfo_h [lindex [.mfo.l.l index active] 0]]>"
		.e.x.t insert end "\n$linktext\n\n" {blue mlfilelink}
	}
}

proc disp_header {header} {
	puts "disp_header"
	set h [header_to_dict $header]
	if { $h == "" } {
		return
	}
	if { [dict get $h type] == "m" } {
		set from [dict get $h from]
		if { $from != $::me } {
			set from [lindex [array get ::contacts "[::sha1::sha1 contact:$from]*"] 1]
		} else {
			set from "me"
		}
		if { $from != "" } {
			set subjtf "$from: personal"
		} else {
			set subjtf "personal"
		}
	} else {
		set to [dict get $h to]
		set subject [dict get $h subject]
		set subjtf "$to: $subject"
	}
	set epoch [dict get $h epoch]
	if { $epoch == "" } {
		set epoch 0
	}
	if { $epoch == "n"} {
		puts "header $h wtf"
		return "error"
	}
	#return [list [clock format $epoch -format {%Y-%m-%d %H:%M:%S}] "$subjtf" [dict get $h hash]]
	return "[clock format $epoch -format {%Y-%m-%d %H:%M:%S}] | [dict get $h type] | [string range [dict get $h hash] 0 3] | $subjtf"
}

proc header_to_dict {header} {
	#puts "header_to_dict"
	if {[string index $header end] != "=" && [string index $header end] != ":"} {
		puts "weird end"
		return
	}
	if {[regexp -all {:} $header] != 11} {
		puts "weird short"
		return
	}
	dict set d hash [lindex [split $header {:}] 0]
	dict set d group [lindex [split $header {:}] 1]
	dict set d len [lindex [split $header {:}] 2]
	dict set d epoch [lindex [split $header {:}] 3]
	dict set d from "[unwrap [lindex [split $header {:}] 4]]"
	dict set d to "[unwrap [lindex [split $header {:}] 5]]"
	dict set d subject "[unwrap [lindex [split $header {:}] 6]]"
	# old news n, old mail m, personal mail p, group mail g
	dict set d type "[lindex [split $header {:}] 7]"
	dict set d nickname "[unwrap [lindex [split $header {:}] 8]]"
	dict set d kfrom "[unwrap [lindex [split $header {:}] 9]]"
	dict set d kto "[unwrap [lindex [split $header {:}] 10]]"
	dict set d gsig "[unwrap [lindex [split $header {:}] 11]]"
	return $d
}

proc dict_to_header {d} {
	#puts "dict_to_header"
	set h {}
	append h [dict get $d hash]
	append h :
	append h [dict get $d group]
	append h :
	append h [dict get $d len]
	append h :
	append h [dict get $d epoch]
	append h :
	append h [wrap [dict get $d from]]
	append h :
	append h [wrap [dict get $d to]]
	append h :
	append h [wrap [dict get $d subject]]
	append h :
	append h [dict get $d type] 
	append h :
	append h [wrap [dict get $d nickname]]
	append h :
	append h [wrap [dict get $d kfrom]]
	append h :
	append h [wrap [dict get $d kto]]
	append h :
	append h [wrap [dict get $d gsig]]
	return $h
}

proc read_headers {waitids} {
	puts "read headers by waitids $waitids"
	if { $waitids == "" } {
		puts "empty waitids, return"
		return
	}
	set hkeys {}
	foreach waitid $waitids {
	foreach {key value} [array get ::waitvalue "[lindex [split $waitid {,}] 0]*"] {
		set skey [lindex [split $key {,}] 0]
		if { $value == "DONE" } {
			puts "type smt DONE $skey"
			lappend hkeys $skey
		}
	}
	}
	set ::msglist_l {}
	#set ::msglist_date {}
	#set ::msglist_subj {}
	#set ::msglist_hash {}
	set ::msglist_t {}
	set ::msglist_k {}
	puts "filling msglist"
	foreach item $hkeys {
		set values [lsort -unique -stride 2 -index end [array get ::headers "$item*"]]
		if { [llength $values] == 0 } {
			continue	
		}
		array unset ::headers "$item*"
		foreach {vkey vvalue} $values {
			puts "type hdrs $vkey $vvalue"
			set d [header_to_dict $vvalue]
			if { $d != "" } {
				array set ::headers [list $vkey $vvalue]
				lappend ::msglist_t $vvalue
				lappend ::msglist_t [dict get [header_to_dict $vvalue] epoch]
			}
		}
	}
	set ::msglist_t [lsort -unique -integer -stride 2 -index end -increasing $::msglist_t]
	foreach {msg epoch} $::msglist_t {
		set hdr [disp_header $msg]
		if { $hdr == "" } {
			continue
		}
		lappend ::msglist_k $msg
		lappend ::msglist_l $hdr
		#lappend ::msglist_l "[lindex $hdr 0] | [string range [lindex $hdr 2] 0 3] | [lindex $hdr 1]"
		#lappend ::msglist_date [lindex $hdr 0]
		#lappend ::msglist_subj [lindex $hdr 1]
		#lappend ::msglist_hash [lindex $hdr 2]
	}
	#puts $::msglist
	#puts $::msglist_k
}

proc disp_contact {contact} {
	puts "disp_contact"
	set c [contact_to_dict $contact]
	if { $c == "" } {
		return
	}
	return "[clock format [dict get $c epoch] -format {%Y-%m-%d %H:%M:%S}] | [string range [::sha1::sha1 $contact] 0 3] | [dict get $c peerid] -> [dict get $c nickname]"
}

proc read_contacts {waitids} {
	puts "read contacts by waitids $waitids"
	if { $waitids == "" } {
		puts "empty waitids, return"
		return
	}
	set hkeys {}
	foreach waitid $waitids {
	foreach {key value} [array get ::waitvalue "$waitid*"] {
		set skey [lindex [split $key {,}] 0]
		if { $value == "DONE" } {
			puts "type smt DONE $skey"
			lappend hkeys $skey
		}
	}
	}
	puts "hkeys $hkeys"
	set ::contactlist {}
	set ::contactlist_k {}
	foreach item $hkeys {
		set c [lsort -unique -stride 2 -index end [array get ::contacts "$item*"]]
		if { [llength $c] == 0 } {
			continue
		}
		array unset ::contacts "$item*"
		foreach {vkey vvalue} $c {
			puts "type cnts $vkey $vvalue"
			set cnt [disp_contact $vvalue]
			if { $cnt != "" } {
				#puts "disp_contact [disp_contact $vvalue]"
				#lappend ::contactlist [disp_contact $vvalue]
				array set ::contacts [list $vkey $vvalue]
				lappend ::contactlist_k $vvalue 
				lappend ::contactlist $cnt 
			}
		}
	}
}

proc disp_group {group} {
	puts "disp_group"
	set g [ml_groupdict $group]
	if { $g == "" } {
		return
	}
	return "[clock format [dict get $g epoch] -format {%Y-%m-%d %H:%M:%S}] | [string range [dict get $g gid] 0 3] | [dict get $g name] - [dict get $g desc]"
}

proc read_groups {waitids} {
	puts "read groups by waitids $waitids"
	if { $waitids == "" } {
		puts "empty waitids, return"
		return
	}
	set hkeys {}
	foreach waitid $waitids {
	foreach {key value} [array get ::waitvalue "$waitid*"] {
		set skey [lindex [split $key {,}] 0]
		if { $value == "DONE" } {
			puts "type smt DONE $skey"
			lappend hkeys $skey
		}
	}
	}
	puts "hkeys $hkeys"
	set ::grouplist {}
	set ::grouplist_k {}
	foreach item $hkeys {
		set g [lsort -unique -stride 2 -index end [array get ::groups "$item*"]]
		if { [llength $g] == 0 } {
			continue
		}
		array unset ::groups "$item*"
		foreach {vkey vvalue} $g {
			puts "type grps $vkey $vvalue"
			set grp [disp_group $vvalue]
			if { $grp != "" } {
				array set ::groups [list $vkey $vvalue]
				lappend ::grouplist_k $vvalue 
				lappend ::grouplist $grp
			}
		}
	}
}

proc show_text {id} {
	if { $id == "" } {
		return
	}
	set ::text $id
}

proc show_headers {ids} {
	if { $ids == "" } {
		return
	}
	set ::search $ids
}

proc show_groups {ids} {
	if { $ids == "" } {
		return
	}
	set ::groupsearch $ids
}

proc show_contacts {ids} {
	if { $ids == "" } {
		return
	}
	set ::contactsearch $ids
}

proc check_waitvalues {} {
	puts "check_waitvalues"
	#puts "waitlast [expr {$::waitlast+500}] > [clock microseconds]"
	if { [expr {$::waitlast+1000}] > [clock microseconds]} {
		return	
	}
	set ::waitlast [clock microseconds]
	set h_updated 0
	set c_updated 0
	set g_updated 0
	set s_updated 0
	foreach {key value} [array get ::waitvalue] {
		# if not WAIT, do nothing
		if { $value != "WAIT" } {
			continue
		}
		puts "waitvalue $key -> $value"
		set hash [lindex [split $key {,}] 0]
		# check if we have received any values by that key
		set found [lsort -unique -stride 2 -index end [array get ::valuestore "$hash*"]]
		set len [llength $found]
		if { $len > 0 } {
			array set ::waitvalue [list $key "DONE"]
		}
		# check if we have in target stores any values by that key
		puts "mytype"
		set type [lindex [array get ::waitvalue "$key,type"] 1]
		set store {}
		switch $type {
			"headers" {	
				puts "type $type"
				set store ::headers
				incr h_updated 1
			}
			"sources" {
				puts "type $type"
				set store ::sources
				incr s_updated 1
			}
			"contacts" {
				puts "type $type"
				set store ::contacts	
				incr c_updated 1
			}
			"groups" {
				puts "type $type"
				set store ::groups
				incr g_updated 1
			}
			default {
				puts "unknown type"
			}
		}
		set lfound {}
		if { $store != "" } {
			set lfound [lsort -unique -stride 2 -index end [array get $store "$hash*"]]
			#puts "type $store lfound $lfound"
			set llen [llength $lfound]
			if { $llen > 0 } {
				puts "found locally"
				array set ::waitvalue [list $key "DONE"]
			}
		} else {
			array set ::waitvalue [list $key "DONE"]
			continue
		}
		# check if we need to do additional actions if these are headers or sources
		puts "values len is $len and type is $type"
		set afound [list {*}$found {*}$lfound]
		puts "set store $store to $afound"
		if { $len > 0 && $type != "" } {
			foreach {skey svalue} $afound {
				if {[string length $svalue] == 0} {
					continue
				}
				puts "add $type $skey -> [string map {{{ {} }} {}} $svalue]"
				set c 0
				foreach item [string map {"{" "" "}" ""} $svalue] {
					puts "add item $item"
					if { [string length "$item"] == 0 } {
						continue
					}
					if { [regexp -all {:} $item] != 0 && $type == "sources" } {	
						continue
					}
					if { $type == "groups" && [ml_groupdict $item] == "" } {
						continue
					}
					if { $type == "contacts" && [contact_to_dict $item] == "" } {
						continue
					}
					if { $type == "headers" && [header_to_dict $item] == "" } {
						continue
					}
					puts "set store $store $skey->$item"
					array set $store [list $skey "$item"]
					array set $store [list $skey,[expr {$c%8}],[expr "[clock microseconds]%8"] "$item"]
					incr c 1
				}
			}
		}
	}
	if { $h_updated > 0 } {
		update_mail {} 
	} 
	if { $c_updated > 0 } {
		update_directory
	}
	if { $g_updated > 0 } {
		update_group_directory
	}
}

proc sol_store {} {
	puts "sol_store"
	foreach store [list ::headers ::contacts ::peerstore] {
	foreach {rkey value} [array get $store] {
		set key [lindex [split $rkey {,}] 0]
		if { $value == "" } {
			array unset $store "$key*"
			continue
		}
		# find 1 peers closest
		set peers [closest_in_buckets $key 1]
		#puts "sol_store to peers $peers"
		# send store to each
		foreach item $peers {
			set speer [split $item {:}]
			set delay [expr "[clock microseconds]%180000"]
			#puts "sol delay $delay"
			after $delay [list str_start [str_create STORE [lindex $speer 1] [lindex $speer 2] $key $value]]
		}
	}
	}
}

proc sc_setsources {} {
	puts "sc_setsources"
	foreach {key val} [array get ::headers] {
		#puts "set sources for $val"
		set hd [header_to_dict $val]
		if { $hd == "" } {
			continue
		}
		set hash [dict get $hd hash]
		set localname [file join $::filepath "mailnews" $hash]
		if { [array names ::file_by_hash $hash] != "" || [file exists $localname] } {
			array set ::sources [list $hash $::me]
		} else {
			array unset ::sources "$hash*"
		}
	}
	foreach {key val} [array get ::file_by_hash] {
		#puts "set sources for $val"
		set d [dl_reqdict $val]
		if { $d == "" } {
			continue
		}
		set hash [dict get $d filehash]
		array set ::sources [list $hash $::me]
	}
	foreach {key val} [array get ::jgroups] {
		set g [ml_groupdict $val]
		if { $g == "" } {
			continue
		}
		set gid [dict get $g gid]
		set src {}
		foreach {id peer} [array get ::sources $gid] {
			lappend src $gid
			lappend src $peer
		}
		lappend src $gid
		lappend src $::me
		set src [lsort -unique -stride 2 -index end $src]
		array set ::sources $src 
	}
}

proc sc_out {header} {
	# 
	puts "sc_out header $header"
	set hd [header_to_dict $header]
	puts "header for keywords : $hd"
	# create array of keywords
	#set k "[split $header {:}]"
	#foreach item $k {
	#	set h [::sha1::sha1 "header:$item"]
	#	lappend s $h 
	#	puts "keyword $item to $h"
	#}
	lappend s [::sha1::sha1 "header:[dict get $hd hash]"]
	if { [dict get $hd type] == "m" } {
		lappend s [::sha1::sha1 "header:[dict get $hd kfrom]"]
		lappend s [::sha1::sha1 "header:[dict get $hd kto]"]
		lappend s [::sha1::sha1 "header:[dict get $hd from]"]
		lappend s [::sha1::sha1 "header:[dict get $hd to]"]
	} else {
		set to [split [dict get $hd to] { }]
		foreach item $to {
			set h [::sha1::sha1 "header:$item"]
			lappend s $h 
			puts "keyword header:$item to $h"
		}
		set from [split [dict get $hd from] { }]
		foreach item $from {
			set h [::sha1::sha1 "header:$item"]
			lappend s $h 
			puts "keyword header:$item to $h"
		}
		set subject [split "[dict get $hd subject]" { }]
		foreach item $subject {
			set h [::sha1::sha1 "header:$item"]
			lappend s $h 
			puts "keyword header:$item to $h"
		}
	}
	foreach key $s {
		# find 4 peers closest
		set peers [closest_in_buckets $key 4]
		puts "sc_out peers $peers"
		# send store to each
		foreach item $peers {
			set speer [split $item {:}]
			str_start [str_create STORE [lindex $speer 1] [lindex $speer 2] $key $header]
		}
		# don't forget to add to our own valuestore
		array set ::headers [list "$key" $header]
		array set ::headers [list "$key,[clock microseconds]" $header]
	}
	sc_setsources
}

proc sc_publishgroup {group} {
	puts "sc_publishgroup group $group"
	set g [ml_groupdict $group]
	lappend s [::sha1::sha1 "group:[dict get $g gid]"]
	# name - why the hell not
	lappend s [::sha1::sha1 "group:[dict get $g name]"]
	lappend s [::sha1::sha1 "group:[dict get $g desc]"]
	foreach w [split [dict get $g name] { }] {
		lappend s [::sha1::sha1 "group:$w"]
	}
	# pkey 
	lappend s [::sha1::sha1 "group:[dict get $g pkey]"]
	foreach key $s {
		# find 4 peers closest
		set peers [closest_in_buckets $key 4]
		puts "sc_publishgroup peers $peers"
		# send store to each
		foreach item $peers {
			set speer [split $item {:}]
			str_start [str_create STORE [lindex $speer 1] [lindex $speer 2] $key $group]
		}
		# don't forget to add to our own valuestore
		array set ::groups [list "$key" $group]
		array set ::groups [list "$key,[clock microseconds]" $group]
	}
	array set ::sources [list "[dict get $g gid]" $::me]
	array set ::sources [list "[dict get $g gid],[clock microseconds]" $::me]
}

proc sc_publishcontact {contact} {
	puts "sc_publishcontact contact $contact"
	set c [contact_to_dict $contact]
	# peerid:b64(pubkey):b64(nickname):age:sex(M/F/O/N):b64(country):b64(city):epoch:b64(sig - sign previous with pubkey):... probable authority pkey&sig
	# peerid - probably needed
	lappend s [::sha1::sha1 "contact:[dict get $c peerid]"]
	# nickname - why the hell not
	lappend s [::sha1::sha1 "contact:[dict get $c nickname]"]
	foreach w [split [dict get $c nickname] { }] {
		lappend s [::sha1::sha1 "contact:$w"]
	}
	# country
	lappend s [::sha1::sha1 "contact:[dict get $c country]"]
	# city 
	lappend s [::sha1::sha1 "contact:[dict get $c city]"]
	foreach key $s {
		# find 4 peers closest
		set peers [closest_in_buckets $key 4]
		puts "sc_publishcontact peers $peers"
		# send store to each
		foreach item $peers {
			set speer [split $item {:}]
			str_start [str_create STORE [lindex $speer 1] [lindex $speer 2] $key $contact]
		}
		# don't forget to add to our own valuestore
		array set ::contacts [list "$key" $contact]
		array set ::contacts [list "$key,[clock microseconds]" $contact]
	}
}

proc enc_msg {key msg init hash} {
	#puts "enc_msg msg $msg"
	#puts "enc_msg key $key msg $msg init $init hash $hash"
	#set start [clock microseconds]
	if { [llength [array names ::keys "myaeskey_$hash"]] == 0 } {
		set init 1
	}
	#puts "start enc_msg $init $start"
	if { $init == 1 } {
		puts "enc_msg $init key $key\n"
		set pubkey [::pki::pkcs::parse_public_key $key]
		set aeskey [read $::randomchan 32]
		array set ::keys [list "myaeskey_$hash" $aeskey]
		set aeskey_e [::pki::encrypt -hex -pub $aeskey $pubkey]
		set nil_block [string repeat \0 16]
		set msg_e [::aes::aes -mode cbc -dir encrypt -key $aeskey -iv $nil_block $msg]
		set msg_s [::pki::sign $msg_e $::mekey]
		set msg_m [::pki::public_key $::mekey]
		set ret "$init,$hash,[wrap $aeskey_e],[wrap $msg_e],[wrap $msg_s],[wrap $msg_m]"
	} else {
		puts "enc_msg $init"
		set aeskey [lindex [array get ::keys "myaeskey_$hash"] 1]
		set nil_block [string repeat \0 16]
		set msg_e [::aes::aes -mode cbc -dir encrypt -key $aeskey -iv $nil_block $msg]
		set ret "$init,$hash,[wrap $msg_e]"
	}
	#set end [clock microseconds]
	#puts "end enc_msg $init $end"
	#puts "time enc_msg $init [expr {$end-$start}]"
	return $ret
}

proc dec_msg {emsg} {
	set s [split $emsg {,}]
	set init [lindex $s 0]
	set hash [lindex $s 1]
	#puts "dec_msg init $init hash $hash"
	if { $init == 1 } {
		set aeskey_e [unwrap [lindex $s 2]]
		set msg_e [unwrap [lindex $s 3]]
		set msg_s [unwrap [lindex $s 4]]
		set msg_m [unwrap [lindex $s 5]]
		#puts "dec_msg msg_e $msg_e msg_s $msg_s msg_m $msg_m"
		set ver [::pki::verify $msg_s $msg_e [::pki::pkcs::parse_public_key $msg_m]]
		if { $ver == "false" } {
			return "sigerr"
		}
		set aeskey [::pki::decrypt -hex -priv $aeskey_e $::mekey]	
		array set ::keys [list "aeskey_$hash" $aeskey]
	} else {
		set msg_e [unwrap [lindex $s 2]]
		set aeskey [lindex [array get ::keys "aeskey_$hash"] 1]
	}
	set nil_block [string repeat \0 16]
	set msg [::aes::aes -mode cbc -dir decrypt -key $aeskey -iv $nil_block $msg_e]
	#puts "dec_msg msg $msg"
	return [string trim $msg]
}

proc chat_add {contact} {
	#set hash [::sha1::sha1 [dist [::sha1::sha1 $contact] [::sha1::sha1 $::mycontact]]]
	set hash [::sha1::sha1 [lsort [list [dict get [contact_to_dict $contact] peerid] $::me]]]
	array set ::buddies [list $hash $contact]
	return $hash
}

proc gchat_notice {gid notice} {
	gchat_send $gid [wrap "$::menick:$::me"] [clock microseconds] "NOTICE" $notice
	#catch { ".g_$gid.o.t" insert end "NS $notice\n" }
	#if { [winfo exists ".g_$gid.o.t"] } {
	#	".g_$gid.o.t" yview 1
	#}
}

proc chat_notice {hash notice} {
	#if { [winfo exists ".c_$hash"] == 0 } {
	#	return
	#}
	chat_send $hash {} "NOTICE" $notice
	#chat_append_notice $hash [clock seconds] $notice
}

proc gchat_sendbutton {gid} {
	#set msg [".g_$gid.i.t" get 1.0 end]
	set msg $::gchat_entry
	set id [gchat_text $gid [wrap "$::menick:$::me"] $msg]
	gchat_append $gid [wrap "$::menick:$::me"] [clock seconds] $msg 0
	gchat_history $gid [wrap "$::menick:$::me"] [clock seconds] $msg 0 $id 
	#".g_$gid.i.t" delete 1.0 end
	#".g_$gid.i.t" mark set insert 1.0
	set ::gchat_entry {}
}

proc chat_sendbutton {hash} {
	#set msg [".c_$hash.i.t" get 1.0 end]
	set msg $::chat_entry
	chat_text $hash $msg
	chat_append $hash [clock seconds] $msg 0
	chat_history $hash [clock seconds] $msg 0 {}
	#".c_$hash.i.t" delete 1.0 end
	#".c_$hash.i.t" mark set insert 1.0
	set ::chat_entry {}
}

proc gchat_text {gid sid msg} {
	return [gchat_send $gid $sid {} "TEXT" $msg]
}

proc chat_text {hash msg} {
	chat_send $hash {} "TEXT" $msg
}

proc chat_offer {hash} {
	chat_send $hash {} "OFFER" $::mycontact 
}

proc im_sendkey {contact} {
	set hash [chat_add $contact]
	set b [contact_to_dict $contact]
	set pubkey [::pki::pkcs::parse_public_key [dict get $b pubkey]]
	puts "im_sendkey pubkey $pubkey\n"
	set aeskey [read $::randomchan 32]
	set aeskey_e [::pki::encrypt -hex -pub $aeskey $pubkey]
	set _e [::pki::encrypt -hex -pub $aeskey $pubkey]
	set sig [::pki::sign $aeskey_e $::mekey]
	array set ::keys "$hash,out" $aeskey
	return [wrap "K $aeskey_e $sig"]
}

proc im_recvkey {hash msg} {
	if { [llength [array names ::buddies $hash]] == 0 } {
		array unset ::chatqueue "$hash*"
		return
	}
	set msg [unwrap $msg]
	set b [contact_to_dict [array get ::buddies $hash]]
	set pubkey [::pki::pkcs::parse_public_key [dict get $b pubkey]]
	set aeskey_e [lindex $msg 0]	
	set sig [lindex $msg 1]	
	set aeskey [::pki::decrypt -hex -priv $aeskey_e $::mekey]
	set ver [::pki::verify $sig $aeskey_e $pubkey]
	if { $ver == "false" } {
		return "sigerr"
	}
	array set ::keys "$hash,in" $aeskey
}

proc im_sendmsg {hash msg} {
	# if no key saved, put error
	set aeskey [lindex [array get ::keys $hash,out] 1]
	if { [string length $aeskey] == 0 } {
		return "no key"
	} 
	set nil_block [string repeat \0 16]
	set msg_e [::aes::aes -mode cbc -dir encrypt -key $aeskey -iv $nil_block $msg]
	return "M $msg_e"
}

proc im_recvmsg {hash msg_e} {
	# if no key saved, put error
	set aeskey [lindex [array get ::keys $hash,in] 1]
	if { [string length $aeskey] == 0 } {
		return "no key"
	} 
	set nil_block [string repeat \0 16]
	set msg [::aes::aes -mode cbc -dir decrypt -key $aeskey -iv $nil_block $msg_e]
	return $msg	
}

proc gchat_send {gid sid id type body} {
	if { $type != "OK" } {
		set id [::sha1::sha1 [clock microseconds]]
	}
	if { [llength [array names ::jgroups $gid]] == 0 } {
		puts "gchat no such $gid in ::jgroups"
		array unset ::gchatqueue "$gid*"
		return
	}
	set body [wrap $body]
	set msg [wrap "$gid $sid $id [clock seconds] $type $body"]
	puts "gchat send to $gid $sid $id [clock seconds] $type"
	puts "gchat send $msg"
	#array set ::gchatqueue [list "$gid,$sid,$id,msg" $msg]
	#after 100 check_gchatqueue
	if { $type != "OK" } {
		#puts "gchat put to queue"
		array set ::gchatqueue [list "$gid,$sid,$id,msg" $msg]
		array set ::gchatqueue [list "$gid,$sid,$id,type" $type]
		after 100 check_gchatqueue
	} else {
		set sid_peer [lindex [split [unwrap $sid] {:}] end]
		#puts "gchat send to sid_peer $sid_peer body $body"
		if { $sid_peer == "" } {
			return
		}
		if { $type == "RENEW" } {
			ml_genc $sid_peer {} {} "GCHAT 0 $msg" 2
		} else {
			ml_genc $sid_peer {} {} "GCHAT 0 $msg" 0
		}
	}
	return $id
}

proc chat_send {hash id type body} {
	if { $type != "OK" } {
		set id [::sha1::sha1 [clock microseconds]]
	}
	set start [clock microseconds]
	#puts "start chat send $id $start ms"
	if { [llength [array names ::buddies $hash]] == 0 } {
		array unset ::chatqueue "$hash*"
		return
	}
	set body [wrap $body]
	set msg "$hash $id [clock seconds] $type $body"
	set b [contact_to_dict [lindex [array get ::buddies $hash] 1]]
	set peerid [dict get $b peerid]
	set pubkey [dict get $b pubkey]
	set emsg {}
	if { [llength [array names ::keys "myaeskey_$hash"]] > 0 && $type != "RENEW" } {
		#puts "encode normal message with peerid $peerid and mode 0"
		catch {
		set emsg [enc_msg $pubkey $msg 0 $hash]
		}
	} else {
		#puts "encode start message with peerid $peerid and pubkey $pubkey and mode 1"
		catch {
		set emsg [enc_msg $pubkey $msg 1 $hash]
		}
	}
	if { $emsg == "" } {
		return
	}
	if { $type != "OK" && $type != "NOTICE" } {
		array set ::chatqueue [list "$hash,$id,msg" $emsg]
		array set ::chatqueue [list "$hash,$id,type" $type]
		array set ::chatqueue [list "$hash,$id,peerid" $peerid]
		array set ::chatqueue [list "$hash,$id,last" [clock seconds]]
		array set ::chatqueue [list "$hash,$id,count" 0]
		set queue [clock microseconds]
		set ttq [expr {$queue-$start}]
		#puts "queue chat send $id $queue ms"
		#puts "time chat send $ttq"
		after 50 check_chatqueue
	} else {
		set peers [lsort -unique -stride 2 -index 1 [array get ::peerstore $peerid]]
		set host {}
		set port {}
		foreach {key val} $peers {
			if { [regexp -all {:} $val] != 2 } {
				continue
			} else {
				set host [lindex [split $val {:}] 1]	
				set port [lindex [split $val {:}] 2]	
			}
		}
		send $host $port "CHAT 0 $emsg"
		#ml_genc $peerid {} {} "CHAT 0 $emsg" 0
	}
	return $id
}

proc gchat_recv {host port msg} {
	puts "gchat_recv $msg"
	set s [split [unwrap $msg] { }]
	set gid [lindex $s 0]
	set sid [lindex $s 1]
	set id [lindex $s 2]
	set epoch [lindex $s 3]
	set type [lindex $s 4]
	set body [unwrap [lindex $s 5]]
	puts "gchat_recv gid $gid sid $sid id $id epoch $epoch type $type"
	if { [string length $body] > [expr "1024*1024*32"] } {
		puts "message bigger than 32 megabyte, ignored"
		return
	}
	puts "gid $gid sid $sid id $id epoch $epoch type $type body $body"
	if { [llength [array names ::jgroups $gid]] == 0 } {
		gchat_send $gid $sid $id "NOCONTACT" {}
		puts "message to strange group"
		return
	} 
	set sc [ml_screen g $gid $host $port]
	if { $sc != 0 } {
		puts "screened out $host $port"
		return
	}
	#
	if { $::group_host_mode == 1 && ( $type == "TEXT" || $type == "SYNC" || $type == "NOTICE" )} {
		set gpeerid [dict get [ml_groupdict $::jgroups($gid)] peerid]
		if { $gpeerid == $::me } {	
		#puts "hostmode resend allow"
		foreach name [array names ::group_to_sig "$gid,*"] {
			set peerid [lindex [split $name {,}] 1]
			#puts "hostmode resend to peerid $peerid len [string length $peerid]"
			if { [string length $peerid] == 40 && $peerid != $::me && [lindex [split [unwrap $sid] {:}] 1] != $peerid } {
				#puts "hostmode resend ml_genc"
				ml_genc $peerid {} {} "GCHAT 0 $msg" 0
			}
		}
		}
	}
	# here we should lookup peerid by host and port, check if it's in group_to_sig,
	# put message to some persistent queue if not, so to not show it before need
	switch $type {
		"NOCONTACT" {
			# dunno what, the user is not in that group
		}
		"TEXT" {
			gchat_append $gid $sid $epoch $body 1
			gchat_history $gid $sid $epoch $body 1 $id
			gchat_send $gid $sid $id "OK" "TEXT $::me" 
			if { [winfo exists ".g_$gid"] == 0 } {
				array set ::gnotices [list "$gid,$sid,$id" $type]
			}
		}
		"NOTICE" {
			gchat_append_notice $gid $sid $epoch $body
			#gchat_history $gid $sid $epoch $body 1 $id
			gchat_send $gid $sid $id "OK" "NOTICE $::me"
			if { [winfo exists ".g_$gid"] == 0 && [lindex $body 0] == "TEXT" } {
				array set ::gnotices [list "$gid,$sid,$id" $type]
			}
		}
		"GET" {
			gchat_send $gid $sid $id "OK" "GET $::me"
			gchat_send $gid $sid {} "GIVE" [dl_read $body]
		}
		"GIVE" {
			after 50 [dl_write $body]
			gchat_send $gid $sid $id "OK" "GIVE $::me"
		}
		"SYNC" {
			if { $::group_sync_allowed == 1 } {
			puts "grecv SYNC $gid"
			gchat_send $gid $sid $id "OK" "SYNC $::me"
			gchat_send $gid $sid {} "ARC" [gchat_history_get $gid [gchat_get_ids $gid 3 10]]
			}
		}
		"ARC" {
			if { $::group_sync_allowed == 1 } {
			puts "grecv ARC $gid"
			set cids [gchat_get_ids $gid 3 {}]
			foreach {msid mepoch mbody mtype mid} $body {
				set msid_peer [lindex [split [unwrap $msid] {:}] end]
				puts "grecv ARC msg msid $msid mepoch $mepoch mbody $mbody mtype $mtype mmid $mid"
				if { $msid_peer != $::me && [lsearch -inline $cids $mid] == {} } {
					puts "grecv ARC ADD msg msid $msid mepoch $mepoch mbody $mbody mtype $mtype mmid $mid"
					puts "grecv ARC res [lsearch -inline $cids $mid] mid $mid not in cids $cids"
					puts "grecv ARC text $mbody"
					gchat_append $gid $msid $mepoch $mbody 1
					gchat_history $gid $msid $mepoch $mbody 1 $mid
				} else {
					puts "grecv ARC wrong msid_peer $msid_peer or lsearch ([lsearch -inline $cids $mid]) must be empty"
				}
			}
			#gchat_history_read $gid
			}
		}
		"RENEW" {
			if { $body == "ask" } {
				gchat_send $gid $sid $id "RENEW" "answer" 
				#ml_genc [lindex $body 1] {} {} "GCHAT 0 RENEW answer" 0
			}
		}
		"OK" {
			set otype [lindex [array get ::gchatqueue "$gid,*,$id,type"] end]
			gchat_append_ok $gid $sid $epoch [lindex $body 0] [lindex $body 1]
			array unset ::gchatqueue "$gid,$sid,$id*"	
		} 
		default {
			puts "unknown gchat message"
			puts "msg $msg"
		}
	}
	after 50 check_gchatqueue
}

proc chat_recv {host port emsg} {
	puts "chat_recv $emsg"
	set start [clock microseconds]
	#puts "start chat recv $start"
	set msg {}
	catch {
	set msg [dec_msg $emsg]
	}
	if { $msg == "" } {
		#if { $first == 1 } {
		#	after 1000 [list chat_recv [list $host $port "re-$emsg"]]
		#}
		return
	}
	#puts "CHATRECV msg $msg"
	set s [split $msg { }]
	#puts "CHATRECV s $s"
	set hash [lindex $s 0]
	set id [lindex $s 1]
	set epoch [lindex $s 2]
	set type [lindex $s 3]
	set body [unwrap [lindex $s 4]]
	puts "chat_recv hash $hash id $id epoch $epoch type $type"
	if { [string length $body] > [expr "1024*1024*32"] } {
		puts "message bigger than 32 megabyte, ignored"
		return
	}
	#puts "CHATRECV hash $hash"
	#puts "CHATRECV id $id"
	#puts "CHATRECV epoch $epoch"
	#puts "CHATRECV type $type"
	#puts "CHATRECV body $body"
	if { [llength [array names ::buddies $hash]] == 0 } {
		chat_send $hash $id "NOCONTACT" {}
		return
	} 
	set sc [ml_screen p [lindex [split $::buddies($hash) {:}] 0] $host $port]
	if { $sc != 0 } {
		puts "screened out $host $port"
		return
	}
	# here we should lookup peerid by host and port, check if it's in person_to_sig,
	# put message to some persistent queue if not, so to not show it before need
	switch $type {
		"NOCONTACT" {
			chat_offer $hash
		}
		"OFFER" {
			if { [llength [array names ::buddies $hash]] == 0 } {
				chat_showoffer $hash $body
			}
			chat_send $hash $id "OK" $::me
		}
		"TEXT" {
			#show_chatwindow $hash
			chat_append $hash $epoch $body 1
			chat_history $hash $epoch $body 1 {} 
			chat_send $hash $id "OK" $::me
			if { [winfo exists ".c_$hash"] == 0 } {
				array set ::notices [list "$hash,$id" $type]
				update_buddies
				#after idle update_buddies
				#after 200 update_buddies
			}
		}
		"NOTICE" {
			#show_chatwindow $hash
			chat_append_notice $hash $epoch $body
			#chat_history $hash $epoch $body 2 {}
			chat_send $hash $id "OK" $::me
			if { [winfo exists ".c_$hash"] == 0 && [lindex $body 0] == "TEXT" } {
				array set ::notices [list "$hash,$id" $type]
				update_buddies
				#after idle update_buddies
				#after 200 update_buddies
			}
		}
		"GET" {
			chat_send $hash $id "OK" $::me
			chat_send $hash {} "GIVE" [dl_read $body]
		}
		"GIVE" {
			after 50 [dl_write $body]
			chat_send $hash $id "OK" $::me
		}
		"RENEW" {
			if { $body == "ask" } {
				chat_send $hash $id "RENEW" "answer"
			}
		}
		"OK" {
			#show_chatwindow $hash
			#chat_append_notice $hash [clock seconds] "Message delivered"	
			set otype [lindex [array get ::chatqueue "$hash,$id,type"] end]
			chat_append_ok $hash $epoch $otype $body
			array unset ::chatqueue "$hash,$id,*"	
		} 
		default {
			puts "unknown chat message"
			puts "emsg $emsg msg $msg"
		}
	}
	set end [clock microseconds]
	#puts "start chat recv $end"
	set trec [expr {$end-$start}]
	#puts "time chat recv $trec"
	after 50 check_chatqueue
}

proc gchat_history {gid sid epoch body type id} {
	set path [file join $::filepath "gchat" "h_$gid"]
	set fchan [open $path a]
	puts $fchan "$sid $epoch [wrap $body] $type $id"
	close $fchan
	return
}

proc gchat_history_get {gid aids} {
	puts "gchat_history_get $gid aids $aids"
	set minepoch [expr "[clock seconds] - 3600*24*7"]
	set path [file join $::filepath "gchat" "h_$gid"]
	if { [file exists $path] == 0 } {
		puts "gchat_history_get no sich path"
		return
	}
	set fchan [open $path r]
	set r {}
	while { [gets $fchan line] >= 0} {
		if { [string length $line] > [expr "1024*512"] } {
			puts "gchat_history_get too big"
			continue
		}
		set l [split $line { }]
		set epoch [lindex $l 1]
		set id [lindex $l 4]
		if { $epoch < $minepoch || $id == "" || $id == "id" } {
			puts "gchat_history_get too old or no id -> id=($id)"
			continue
		}	
		set sid [lindex $l 0]
		set body [unwrap [lindex $l 2]]
		set type [lindex $l 3]
		if { [lsearch -inline -all $aids $id] != "" } {
			lappend r $sid 
			lappend r $epoch
			lappend r $body
			lappend r $type
			lappend r $id
		}
	}
	puts "gchat_history_get ret $r"
	return $r
}

proc gchat_get_ids {gid interval num} {
	puts "gchat_get_ids $gid"
	if { $interval == "" } {
		set inteval 7 
	}	
	set minepoch [expr "[clock seconds] - 3600*24*$interval"]
	set r {}
	set path [file join $::filepath "gchat" "h_$gid"]
	if { [file exists $path] == 0 } {
		puts "gchat_get_ids no such path"
		return
	}
	set fchan [open $path r]
	puts "gchat_get_ids opened file"
	while { [gets $fchan line] >= 0} {
		if { [string length $line] > [expr "1024*512"] } {
			puts "gchat_get_ids too big"
			continue
		}
		set l [split $line { }]
		set epoch [lindex $l 1]
		set id [lindex $l 4]
		if { $epoch < $minepoch || $id == "" || $id == "id" } {
			puts "gchat_get_ids too old or no id -> id=($id)"
			continue
		}
		lappend r $id
		lappend r $epoch
	} 
	puts "gchat_get_ids r $r" 
	set lr {}
	foreach {rid repoch} [lsort -unique -integer -stride 2 -index 1 -decreasing $r] {
		lappend lr $rid
	}
	puts "gchat_get_ids lr $lr" 
	if { $num == "" } {
		set $num "end"
	}
	set ret [lrange $lr 0 $num]
	puts "gchat_get_ids ret $ret"
	return $ret
}

proc gchat_history_read {gid} {
	if { [winfo exists ".g_$gid"] == 0 } {
		return
	}
	set path [file join $::filepath "gchat" "h_$gid"]
	if { [file exists $path] == 0 } {
		return
	}
	set minepoch [expr "[clock seconds] - 3600*24*7"]
	set fchan [open $path r]
	while { [gets $fchan line] >= 0} {
		if { [string length $line] > [expr "1024*512"] } {
			continue
		}
		set l [split $line { }]
		set sid [lindex $l 0]
		set epoch [lindex $l 1]
		if { $epoch < $minepoch } {
			continue
		}
		set body [unwrap [lindex $l 2]]
		set type [lindex $l 3]
		set sid_peer [lindex [split [unwrap $sid] {:}] end]
		if { $type == 1 && $sid_peer != $::me } {
			set nickname [unwrap $sid]
			set dir "*[clock format $epoch -format {%Y-%m-%d %H:%M:%S}]*:"		
			".g_$gid.o.t" insert end "<$nickname>$dir\n$body\n"
		} elseif { $type == 0 && $sid_peer == $::me } {
			set nickname $::menick
			set dir "*[clock format $epoch -format {%Y-%m-%d %H:%M:%S}]*:"
			".g_$gid.o.t" insert end "<$nickname>$dir\n$body\n"
		}
	}
	close $fchan
	".g_$gid.o.t" insert end "\n***\n\n"
	".g_$gid.o.t" yview moveto 1
	return
}

proc gchat_append {gid sid epoch body type} {
	set sid_peer {}
	catch {
	set sid_peer [lindex [split [unwrap $sid] {:}] end]
	}
	if { $type == 1 && $sid_peer != $::me } {
		set nickname [unwrap $sid] 
		set tag red
		#set dir "([clock format [clock seconds] -format {%H:%M:%S}])<-"
		set dir "*[clock format $epoch -format {%H:%M:%S}]*:"
	} elseif { $type != 1 && $sid_peer == $::me }  {
		set nickname $::menick
		set tag green
		#set dir "(me)->"
		set dir "*[clock format $epoch -format {%H:%M:%S}]*:"
	} else {
		#puts "ignore own message echo"
		set nickname "weirdo"
		set tag yellow
		set dir "*[clock format $epoch -format {%H:%M:%S}]*:"
		#return
	}

	if { [winfo exists ".g_$gid"] == 1 } {
		".g_$gid.o.t" tag configure red -foreground {#c06060} -font {Sans 9}
		".g_$gid.o.t" tag configure cyan -foreground {#60c0c0} -font {Sans 9}
		".g_$gid.o.t" tag configure green -foreground {#60c060} -font {Sans 9}
		".g_$gid.o.t" tag configure yellow -foreground {#c0c060} -font {Sans 9}
		".g_$gid.o.t" insert end "<$nickname>$dir\n" $tag 
		".g_$gid.o.t" insert end "$body\n"
		".g_$gid.o.t" yview moveto 1
	}

	#set path [file join $::filepath "gchat" $gid]
	#set fchan [open $path a]
	#puts $fchan "<$nickname>$dir $body"
	#close $fchan
	return
}

proc chat_history {hash epoch body type id} {
	set path [file join $::filepath "chat" "h_$hash"]
	set fchan [open $path a]
	puts $fchan "$epoch [wrap $body] $type $id"
	close $fchan
	return
}

proc chat_history_read {hash} {
	if { [winfo exists ".c_$hash"] == 0 } {
		return
	}
	set path [file join $::filepath "chat" "h_$hash"]
	if { [file exists $path] == 0 } {
		return
	}
	set minepoch [expr "[clock seconds] - 3600*24*7"]
	set fchan [open $path r]
	while { [gets $fchan line] >= 0} {
		if { [string length $line] > [expr "1024*512"] } {
			continue
		}
		set l [split $line { }]
		set epoch [lindex $l 0]
		if { $epoch < $minepoch } {
			continue
		}
		set body [unwrap [lindex $l 1]]
		set type [lindex $l 2]
		if { $type == 1 } {
			set b [contact_to_dict [lindex [array get ::buddies $hash] 1]]
			set nickname [dict get $b nickname]
			set dir "*[clock format $epoch -format {%Y-%m-%d %H:%M:%S}]*:"		
			".c_$hash.o.t" insert end "<$nickname>$dir\n$body\n"
		} elseif { $type == 0 }  {
			set nickname $::menick
			set dir "*[clock format $epoch -format {%Y-%m-%d %H:%M:%S}]*:"
			".c_$hash.o.t" insert end "<$nickname>$dir\n$body\n"
		}
	}
	close $fchan
	".c_$hash.o.t" insert end "\n***\n\n"
	".c_$hash.o.t" yview moveto 1
	return
}

proc chat_append {hash epoch body type} {
	if { $type == 1 } {
		set b [contact_to_dict [lindex [array get ::buddies $hash] 1]]
		set nickname [dict get $b nickname]
		set tag red
		#set dir "([clock format [clock seconds] -format {%H:%M:%S}])<-"
		set dir "*[clock format [clock seconds] -format {%H:%M:%S}]*:"
	} else {
		set nickname $::menick
		set tag green
		#set dir "(me)->"
		set dir "*[clock format [clock seconds] -format {%H:%M:%S}]*:"
	}

	if { [winfo exists ".c_$hash"] == 1 } {
		".c_$hash.o.t" tag configure red -foreground {#c06060} -font {Sans 9}
		".c_$hash.o.t" tag configure cyan -foreground {#60c0c0} -font {Sans 9}
		".c_$hash.o.t" tag configure green -foreground {#60c060} -font {Sans 9}
		".c_$hash.o.t" tag configure yellow -foreground {#c0c060} -font {Sans 9}
		".c_$hash.o.t" insert end "<$nickname>$dir\n" $tag 
		".c_$hash.o.t" insert end "$body\n"
		".c_$hash.o.t" yview moveto 1
	}
	
	#set path [file join $::filepath "chat" $hash]
	#set fchan [open $path a]
	#puts $fchan "<$nickname>:$dir $body\n"
	#close $fchan
	return
}

proc gchat_history_append {gid} {
	if { [winfo exists ".g_$gid"] == 0 } {
		return
	}
	set path [file join $::filepath "gchat" $gid]
	if { [file exists $path] == 0 } {
		return
	}
	".g_$gid.o.t" delete 1.0 end
	set fchan [open $path r]
	while { [gets $fchan line] >= 0} {
		".g_$gid.o.t" insert end "$line\n"
	}
	close $fchan	
	".g_$gid.o.t" yview moveto 1
}

proc chat_history_append {hash} {
	if { [winfo exists ".c_$hash"] == 0 } {
		return
	}
	set path [file join $::filepath "chat" $hash]
	if { [file exists $path] == 0 } {
		return
	}
	".c_$hash.o.t" delete 1.0 end
	set fchan [open $path r]
	while { [gets $fchan line] >= 0} {
		".c_$hash.o.t" insert end "$line\n"
	}
	close $fchan	
	".c_$hash.o.t" yview moveto 1
}

proc gchat_append_ok {gid sid epoch otype body} {
	set sid_peer {}
	catch {
	set sid_peer [lindex [split [unwrap $sid] {:}] end]
	}
	if { $sid_peer == $::me } { set sid_peer "me" }
	#puts "gchat_append_ok $gid $sid $epoch $body"
	if { [winfo exists ".g_$gid"] == 1 && $otype == "TEXT" } {
		".g_$gid.o.t" tag configure ok -foreground {#6060c0} -font {Sans 9}
		".g_$gid.o.t" insert end "[clock format [clock seconds] -format {%H:%M:%S}] delivered $otype from $sid_peer to $body\n" {ok}
		".g_$gid.o.t" yview moveto 1
	}
}

proc gchat_append_notice {gid sid epoch body} {
	#if { [string length $body] > [expr "1024*512"] } {
	#	puts "message bigger than 512kb, ignored"
	#	return
	#}
	set kind [lindex [split $body] 0]
	puts "notice kind $kind"
	if { $kind == "FILE" } {
		set full [lindex [split $body {<>}] end-1]
	} elseif { $kind == "IMG" } {
		#return
		puts "appending image"
		set imgdata [::base64::decode [lindex [split $body {<>}] end-1]]
		set img [image create photo -format PNG -data $imgdata]
		set imgdesc "[lindex [split $body {<>}] 0], res: [image width $img]x[image height $img] :"
	} elseif { $kind == "NOTICE" } {
	} else {
		puts "not a right notice"
		return
	}

	if { [winfo exists ".g_$gid"] == 1 } {
		".g_$gid.o.t" tag configure cyan -foreground cyan -font {Sans 9}
		".g_$gid.o.t" tag configure blue -foreground blue -font {Sans 9}
		".g_$gid.o.t" tag configure magenta -foreground magenta -font {Sans 9}
		if { $kind == "NOTICE" } {
		#".g_$gid.o.t" insert end "$kind: *[clock format $epoch -format {%Y-%m-%d %H:%M:%S}]*: $body\n" {blue}
		} elseif { $kind == "FILE" } {
		".g_$gid.o.t" insert end "<[unwrap $sid]>: *[clock format $epoch -format {%Y-%m-%d %H:%M:%S}]*:\n" {blue}
		".g_$gid.o.t" insert end "$full" {blue filelink}
		".g_$gid.o.t" insert end "\n"
		} elseif { $kind == "IMG" } {
		".g_$gid.o.t" insert end "$kind:*[clock format $epoch -format {%Y-%m-%d %H:%M:%S}]*:\n" {magenta}
		".g_$gid.o.t" insert end "$imgdesc\n"
		".g_$gid.o.t" image create end -image $img
		".g_$gid.o.t" insert end "\n"
		}
		".g_$gid.o.t" yview moveto 1
	}
	if { $kind == "STATUS" } {
		return
	}
	#set path [file join $::filepath "gchat" $gid]
	#set fchan [open $path a]
	#puts $fchan "$kind: [clock format $epoch -format {%Y-%m-%d %H:%M:%S}]([clock format [clock seconds] -format {%H:%M:%S}])-> $body\n"
	#close $fchan	
	return
}

proc chat_append_ok {hash epoch otype body} {
	#puts "chat_append_ok $hash $epoch $body"
	if { [winfo exists ".c_$hash"] == 1 && $otype == "TEXT" } {
		".c_$hash.o.t" tag configure ok -foreground "#6060c0" -font {Sans 9}
		".c_$hash.o.t" insert end "[clock format [clock seconds] -format {%H:%M:%S}] delivered $otype to $body\n" {ok}
		".c_$hash.o.t" yview moveto 1
	}
}

proc chat_append_notice {hash epoch body} {
	#if { [string length $body] > [expr "1024*512"] } {
	#	puts "message bigger than 512kb, ignored"
	#	return
	#}
	set kind [lindex [split $body] 0]
	puts "notice kind $kind"
	if { $kind == "FILE" } {
		set full [lindex [split $body {<>}] end-1]
	} elseif { $kind == "IMG" } {
		return
		#puts "appending image"
		set imgdata [::base64::decode [lindex [split $body {<>}] end-1]]
		set img [image create photo -format PNG -data $imgdata]
		set imgdesc "[lindex [split $body {<>}] 0], res: [image width $img]x[image height $img] :"
	} elseif { $kind == "NOTICE" } {
	} else {
		puts "not a right notice"
		return
	}

	if { $kind == "STATUS" } {
		set type [lindex [split [lindex [split $body { }] 1] {/}] 0] 
		set comment [lindex [split [lindex [split $body { }] 1] {/}] 1] 
		set_buddy_status $hash $type $comment 0
	}
	
	if { [winfo exists ".c_$hash"] == 1 } {
		".c_$hash.o.t" tag configure cyan -foreground cyan -font {Sans 9}
		".c_$hash.o.t" tag configure blue -foreground blue -font {Sans 9}
		".c_$hash.o.t" tag configure magenta -foreground magenta -font {Sans 9}
		if { $kind == "NOTICE" } {
		".c_$hash.o.t" insert end "$kind:*[clock format $epoch -format {%Y-%m-%d %H:%M:%S}]*: $body\n" {blue}
		} elseif { $kind == "FILE" } {
		".c_$hash.o.t" insert end "$kind:*[clock format $epoch -format {%Y-%m-%d %H:%M:%S}]*: $body\n" {blue}
		".c_$hash.o.t" insert end "$full" {blue filelink}
		".c_$hash.o.t" insert end "\n"
		} elseif { $kind == "IMG" } {
		".c_$hash.o.t" insert end "$kind:*[clock format $epoch -format {%Y-%m-%d %H:%M:%S}]*:\n" {magenta}
		".c_$hash.o.t" insert end "$imgdesc\n"
		".c_$hash.o.t" image create end -image $img
		".c_$hash.o.t" insert end "\n"
		}
		".c_$hash.o.t" yview moveto 1
	}
	if { $kind == "STATUS" } {
		return
	}
	#set path [file join $::filepath "chat" $hash]
	#set fchan [open $path a]
	#puts $fchan "$kind: [clock format $epoch -format {%Y-%m-%d %H:%M:%S}]([clock format [clock seconds] -format {%H:%M:%S}])-> $body\n"
	#close $fchan	
	return
}

proc check_gchatqueue {} {
	puts "check_gchatqueue"
	if { [expr {$::gchqlast+200}] > [clock microseconds] } {
		return	
	}
	set ::gchqlast [clock microseconds]
	foreach {gid grp} [array get ::jgroups] {
		set msgs [array get ::gchatqueue "$gid*msg"]
		if { [llength $msgs] == 0 } {
			puts "no msgs in $gid"
			continue
		}
		set gpeerid [dict get [ml_groupdict $::jgroups($gid)] peerid]
		if { $::group_host_mode != 1 || $gpeerid == $::me} {
			set gsrc [lrange [ml_get_srcs $gid] 2 end]
		} else {
			set gsrc $gpeerid
		}
		puts "gchatqueue gsrc $gsrc"
		foreach {key msg} $msgs {
		set rkey [string map {{,msg} {}} $key]
		foreach pv $gsrc {
			if { [string length $pv] != 40 } {
				puts "pv $pv wrong length"
				continue
			}
			puts "pv $pv"
			set found [lsort -unique -stride 2 -index end [array get ::peerstore "$pv*"]]
			puts "pv found $found"
			if {[llength $found] == 0} {
				#puts "find sources for $gid"
				#after 100 [list sc_get_sources $gid]
				set peers [closest_in_buckets $pv 2]
				puts "gchat peers $peers"
				foreach peer $peers {
					set speer [split $peer {:}]
					puts "asking $peer for our gchat buddy"
					if {[llength $speer] == 3} {
						str_start [str_create FIND_NODE [lindex $speer 1] [lindex $speer 2] $pv none]
					}
				}
			}
			foreach {psk psv} $found {
				if { [string length $psk] != 40 } {
					continue
				}
				set rkey [string map {{,msg} {}} $key]
				#puts "msg $msg"
				set speer [split $psv {:}]
				#puts "speer $speer"
				set peerid [lindex $speer 0]
				#puts "peerid $peerid"
				set host [lindex $speer 1]
				#puts "host $host"
				set port [lindex $speer 2]
				#puts "port $port"
				puts "gchatqueue send $peerid $host $port GCHAT 0 $msg"
				#send $host $port "GCHAT 0 $msg"
				ml_genc $peerid $host $port "GCHAT 0 $msg" 0
			}
		}
		puts "gchatqueue unset $rkey"
		array unset ::gchatqueue "$rkey*"
		}
	}
}

proc check_chatqueue {} {
	puts "check_chatqueue"
	if { [expr {$::chqlast+100}] > [clock microseconds] } {
		return	
	}
	set ::chqlast [clock microseconds]
	set enterq $::chqlast
	#puts "start chatq $enterq"
	foreach {hash buddy} [array get ::buddies] {
		foreach {key value} [array get ::chatqueue "$hash*peerid"] {
			set found [array get ::peerstore "$value*"]
			#puts "found chat peers [llength $found] elements (all) -> $found"
			if {[llength $found] > 0} {
					set rkey [string map {{,peerid} {}} $key]
					set msg [lindex [array get ::chatqueue "$rkey,msg"] 1]
					set speer [split [lindex $found 1] {:}]
					set peerid [lindex $speer 0]
					set host [lindex $speer 1]
					set port [lindex $speer 2]
					set last [lindex [array get ::chatqueue "$rkey,last"] 1]
					set count [lindex [array get ::chatqueue "$rkey,count"] 1]
					if { $last != "" } {
						set now [clock seconds]
						set old [expr {$now-$last}]
					} else {
						set old 0	
						set count 0
					}
					if { $count == 0 || ($old > 3 && $count == 1) } {
						#tcp_send $host $port "CHAT 0 $msg"
						send $host $port "CHAT 0 $msg"
						#ml_genc $peerid $host $port "CHAT 0 $msg" 0
					} else {
						array unset ::chatqueue "$rkey*"
					}
					array set ::chatqueue [list "$rkey,last" [clock seconds]]
					array set ::chatqueue [list "$rkey,count" [expr {$count+1}]]
			} else {	
					set peers [closest_in_buckets [lindex [split $value { }] 0] 2]
					puts "chat peers $peers"
					foreach peer $peers {
						set speer [split $peer {:}]
						puts "asking $peer for our chat buddy"
						if {[llength $speer] == 3} {
							str_start [str_create FIND_NODE [lindex $speer 1] [lindex $speer 2] $value none]
						}
					}
			}
		}
	}
	set end [clock microseconds]
	#puts "end chatq $end"
	#puts "time chatq [expr {$end-$enterq}]"
}

proc check_peers {} {
	puts "check_peers and buckets"
	#puts "peerlast [expr {$::peerlast+200}] > [clock microseconds]"
	if { [expr {$::peerlast+200}] > [clock microseconds]} {
		return	
	}
	set ::peerlast [clock microseconds]
	#puts "check peers and buckets"
	foreach {bkey bvalue} [array get ::b] {
		if {[llength [array names ::peerstore [lindex [split $bkey {,}] 1]]] == 0} {
			array unset ::b $bkey
		}
	}
	set inbuckets [llength [array names ::b]]
	set inpeerstore [llength [array names ::peerstore]]
	if { $inbuckets < 20 && $inbuckets < $inpeerstore } {
		foreach {pkey pvalue} [array get ::peerstore] {
			set speer [split $pvalue {:}]
			if { [array get ::b "*,$pkey"] == "" } {
				str_start [str_create PING [lindex $speer 1] [lindex $speer 2] $pkey none]
			}
		}
	}
	array unset ::peerstore "$::me*"
	array unset ::b "*,$::me*"
}

proc update_groups {} {
	if { [winfo exists .g] == 0 && [winfo exists .sgs] == 1} {
		return
	}
	set ::jgroups_l {}
	set ::jgroups_i {}
	foreach {key value} [array get ::jgroups] {
		set b [ml_groupdict $value]
		lappend ::jgroups_l "[dict get $b name] <[dict get $b gid]>"
		lappend ::jgroups_i $key
	}
	after 3000 update_groups
}

proc update_buddies {} {
	if { [winfo exists .b] == 0 && [winfo exists .sbs] == 1} {
		return
	}
	set ::peernum [llength [lsort -unique [array names ::b]]]
	set ::buddies_l {}
	set ::buddies_k {}
	#if { [.b.l.tv exists {buddies}] } {
	#	.b.l.tv delete {buddies}
	#}
	#.b.l.tv insert {} 0 -id {buddies} -text {buddies} -open true 
	#.b.l.tv item delete all
	#.b.l.tv column delete all
	#set colid_name [.b.l.tv column create -text {name}]
	#set colid_n [.b.l.tv column create -text {n}]
	#set colid_s [.b.l.tv column create -text {s}]
	#set colid_peer [.b.l.tv column create -text {id}]
	#.b.l.tv configure -treecolumn $colid_name
	#.b.l.tv element create border border -background {#ece9d8} -filled yes -relief solid -thickness 1
	#.b.l.tv element create buddy text
	#.b.l.tv style create def_style 
	#.b.l.tv style elements def_style {buddy border}
	#.b.l.tv style layout def_style border -union {buddy} -ipadx 0 -ipady 0
	#.b.l.tv style layout def_style buddy
	#.b.l.tv item configure root -button yes
	#.b.l.tv item style set root $colid_name def_style
	#.b.l.tv item element configure root $colid_name buddy -text {buddies}
	#set c 1
	foreach {key value} [array get ::buddies] {
		set b [contact_to_dict $value]
		set n [llength [array names ::notices "$key*"]]
		lappend ::buddies_l "[lindex [array get ::statuses $key,type] 1] ($n) [dict get $b nickname] <[dict get $b peerid]>"
		lappend ::buddies_k $key
		#.b.l.tv insert {buddies} $c -id $key -values [list "($n)" [dict get $b nickname] "<[dict get $b peerid]>"]
		#set itemid [.b.l.tv item create]
		#.b.l.tv item style set $itemid $colid_name def_style
		#.b.l.tv item style set $itemid $colid_n def_style
		#.b.l.tv item style set $itemid $colid_s def_style
		#.b.l.tv item style set $itemid $colid_peer def_style
		#.b.l.tv item element configure $itemid $colid_name buddy -text [dict get $b nickname] 
		#.b.l.tv item element configure $itemid $colid_n buddy -text $n
		#.b.l.tv item element configure $itemid $colid_s buddy -text [lindex [array get ::statuses $key,type] 1]
		#.b.l.tv item element configure $itemid $colid_peer buddy -text [dict get $b peerid]
		#.b.l.tv item lastchild root $itemid
		#incr c 1
	}
	after 3000 update_buddies
}

proc set_buddy_status {hash type comment last} {
	puts "set_buddy_status $hash $type $comment"
	set epoch [clock seconds]
	set oldepoch [lindex [array get ::statuses $hash,epoch] 1]
	if { $oldepoch == $last || $last == 0 } {
		#puts "set status"
		array set ::statuses [list $hash,type $type ]
		array set ::statuses [list $hash,comment $comment ]
		array set ::statuses [list $hash,epoch $epoch ]
	}
	if { $type != " " } {
		#puts "schedule timeout"
		# 5 minutes
		after 300000 [list set_buddy_status $hash " " "offline" $epoch]
	}
}

proc send_my_status {type comment} {
	puts "send_my_status $type $comment"
	foreach hash [array names ::buddies] {
		#puts "status->chat_notice $hash {STATUS $type/$comment}"
		#chat_send $hash [clock microseconds] "RENEW" "ask"
		chat_notice $hash "STATUS $type/$comment"
	}
	set sid [wrap "$::menick:$::me"]
	foreach gid [array names ::jgroups] {
		#set type "RENEW"
		#set body "ask"
		#set id [clock microseconds]
		#set msg [wrap "$gid $sid $id [clock seconds] $type $body"]
		#ml_genc  $sid [clock microseconds] $msg 0
		gchat_notice $gid "STATUS $type/$comment"
	}
}

proc sc_stop {ids} {
	set ::search {}
	set ::text {}
	if { $ids == "" } {
		puts "empty ids, return"
		return
	}
	foreach id $ids {
		#foreach item [array names ::waitvalue "$id*"] {
		#	array set ::waitvalue [list $item "DONE"]
		#}
		after 5000 [list array unset ::waitvalue "$id*"]
	}
	after 200 check_waitvalues
}

proc sc_get_sources {hash} {
	puts "sc_get_sources $hash"
	if { $hash == "" } {
		puts "sc_get_sources empty key, return"
		return
	}
	set wv_existing [array names ::waitvalue $hash]
	if { $wv_existing != "" } {
		puts "sc_get_sources already looking for this"
		return
	}
	set waitid [sc_get_value $hash]
	# bullshit for now
	# as in - wait for header to 
	# after get_value on sha1 hash of full message,
	# display waiting status -> not here, in display proc

	# wait for value by handle -> just set type

	# value should be interpreted as a list of sources in triples, sources should be 
	#	added to source store with (sha1 of file,sha1 of source) as key -> not here, in strategy for SOURCES response
	#		or ignored 
	array set ::waitvalue [list "$waitid,type" "sources"]
	# schedule that (have a scheduler array?)
	return $waitid
}

proc sc_get_groups {keys} {
	puts "sc_get_groups"
	if { $keys == "" || [llength $keys] == 0} {
		puts "empty keys, return"
		return
	}
	set r {}
	foreach key $keys {
		set waitid [sc_get_value $key]
		array set ::waitvalue [list "$waitid,type" "groups"]
		lappend r $waitid
	}
	return $r
}

proc sc_get_contacts {keys} {
	puts "sc_get_contacts"
	if { $keys == "" || [llength $keys] == 0} {
		puts "empty keys, return"
		return
	}
	set r {}
	foreach key $keys {
		set waitid [sc_get_value $key]
		array set ::waitvalue [list "$waitid,type" "contacts"]
		lappend r $waitid
	}
	return $r
}

proc sc_get_headers {keys} {
	puts "sc_get_headers"
	if { $keys == "" || [llength $keys] == 0} {
		puts "empty key, return"
		return
	}
	set r {}
	foreach key $keys {
	set waitid [sc_get_value $key]
	# bullshit for now
	# wait for get_value to add header
	# after get value on keyword,
	# display waiting status

	# wait for value by handle
	
	# value should be interpreted as a "sha1:chunksize:chunks:epoch:from:to:subject" without content 
	#		in b64 string, or ignored
	#		when interpreted, added to store of search results by key (search id,handle)
	array set ::waitvalue [list "$waitid,type" "headers"]
	# schedule that (have a scheduler array?)
	lappend r $waitid
	}
	return $r
}

proc sc_get_value {key} {
	puts "sc_get_value $key"
	if { $key == "" } {
		puts "empty key, return"
		return
	}
	# try to find locally
	#if { [llength [array names ::valuestore "$key*"]] > 0} {
	#	puts "array names [array get ::valuestore "$key*"]"
	#	set id "$key,[clock microseconds]"
	#	array set ::waitvalue [list $id "DONE"]
	#	return $id
	#}	
	# commented, because it was bad, wrong and should be done in
	# check_waitvalues only
	# find 4 peers closest
	set peers [closest_in_buckets $key 4]
	# send find_value to each
	foreach peer $peers {
		set speer [split $peer {:}]
		str_start [str_create FIND_VALUE [lindex $speer 1] [lindex $speer 2] $key none]
	}
	# display waiting status <- not here
	
	# add it to waiting to be fetched things array <- that's ok
	set id "$key,[clock microseconds]"
	array set ::waitvalue [list $id "WAIT"]
	after 180000 [list array set ::waitvalue [list $id "DONE"]]
	after 200 check_waitvalues
	# return array key (key and time)
	return $id
}

proc sc_get_values {keys} {
	puts "sc_get_values"
	set ret {}
	foreach key $keys {
		lappend ret [sc_get_value $key]
	}
	after 200 check_waitvalues
	return $ret
}

proc prep_group_keys {s} {
	if { [llength $s] == 0 } {
		puts "no keys, return"	
		return
	}
	set ret {}
	foreach token $s {
		set k [::sha1::sha1 "group:$token"]
		puts "token $token to keyword $k"
		lappend ret $k
	}
	return $ret
}

proc prep_contact_keys {s} {
	if { [llength $s] == 0 } {
		puts "no keys, return"	
		return
	}
	set ret {}
	foreach token $s {
		set k [::sha1::sha1 "contact:$token"]
		puts "token $token to keyword $k"
		lappend ret $k
	}
	return $ret
}

proc build_personal_filter {} {
	set s {}
	lappend s $::me
	lappend s [::pki::public_key $::mekey]
	lappend s $::mycontact
	puts "filter is $s"
	return $s
}

proc prep_header_keys {s} {
	if { [llength $s] == 0 } {
		puts "no keys, return"	
		return
	}
	set ret {}
	foreach token $s {
		set k [::sha1::sha1 "header:$token"]
		puts "token $token to keyword $k"
		lappend ret $k
	}
	return $ret
}

proc update_widgets {} {
	if { [winfo exists .p] == 0 } {
		return
	}
	#puts "update widgets"
	set ::p_l {}
	set ::b_l {}
	set ::peerstore_l {}	
	set ::valuestore_l {}	
	set ::contacts_l {}
	set ::headers_l {}
	set ::waitvalue_l {}
	foreach {sreq req} [array get ::p "*,req" ] {
		set s [lindex [split $sreq {,}] 0]
		set line "# $s | $::p($s,req) | $::p($s,state) | $::p($s,host) | $::p($s,port) | $::p($s,key) | $::p($s,value)" 
		lappend ::p_l $line	
	}
	foreach {bkey seen} [array get ::b ] {
		set key [lindex [split $bkey {,}] 1]
		set line "$key | $seen"
		lappend ::b_l $line
	}
	foreach {key triple} [array get ::peerstore ] {
		set line "$key | $triple"
		lappend ::peerstore_l $line
	}
	foreach {key value} [array get ::valuestore ] {
		set line "$key | $value"
		lappend ::valuestore_l $line
	}
	foreach {key value} [array get ::contacts ] {
		set line "$key | $value"
		lappend ::contacts_l $line
	}
	foreach {key value} [array get ::headers ] {
		set line "$key | $value"
		lappend ::headers_l $line
	}
	foreach {key value} [array get ::waitvalue ] {
		set line "$key | $value"
		lappend ::waitvalue_l $line
	}
	after 1000 update_widgets
}

# send

proc crc_wrap {msg} {
	#set start [clock microseconds]
	set sum [crc::crc32 -format %X $msg]
	#set end [clock microseconds]
	#puts "time crc_wrap [expr {$end-$start}]"
	#puts "crc_wrap $sum"
	return "$sum $msg"
}

# record to fifo, packetize out
# put packets to fifo, play

# start streams, start io
proc audio_start {host port} {
	puts "audio_start"
	set ::audio_in_fifo [fifo]	
	set ::audio_out_fifo [fifo]
	fconfigure $::audio_in_fifo -translation binary -encoding binary
	fconfigure $::audio_out_fifo -translation binary -encoding binary
	snack::sound si -channel $::audio_in_fifo -encoding Mulaw -channels 1 -rate 8000
	snack::sound so -channel $::audio_out_fifo -encoding Mulaw -channels 1 -rate 8000
	set ::audio_host $host
	set ::audio_port $port
	set ::audio_state 2
	if { $::audio_port == 8888 } {
		so record
	} else {
		si play -blocking 0
	}
	set ::audio_schedule [after 1 audio_run]
}

proc audio_run {} {
	puts "audio_run"
	if { $::audio_state != 2 } {
		puts "state not 2"
		return
	} 
	if { [winfo exists .w_audio_run] == 0 } {
		show_audio_run $::audio_host $::audio_port
	}
	set part [read $::audio_out_fifo 512]
	set p [binary format H1a* 4 $part]
	rawsend $::audio_host $::audio_port [wrap $p]
	catch {after cancel $::audio_schedule}
	set ::audio_schedule [after 1 audio_run]
}

proc show_audio_run {host port} {
	set w .w_audio_run
	if { [winfo exists $w] == 1 } {
		return
	}
	toplevel $w
	wm title $w "Call"
	pack [panedwindow "$w.p" -ori vert] -fill both -expand 1
	"$w.p" add [frame "$w.t"] -stretch never
	pack [label "$w.t.l" -text "Call" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	"$w.p" add [frame "$w.w"] -stretch never
	#pack [canvas "$w.w.c" -width 800 -height 600 -highlightthickness $::line_th -highlightcolor red -selectforeground green]
	#"$w.w.c" create waveform 0 0 -sound so -subsample 16 -width 480 -height 320 -pixelspersecond 8
	"$w.p" add [frame "$w.b"] -stretch never
	pack [button "$w.b.d" -text "stop" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "audio_end $host $port; destroy $w"] -fill both -side left
}

proc show_audio {} {
	set w .w_audio
	if { [winfo exists $w] == 1 } {
		return
	}
	toplevel $w
	wm title $w "Phone"
	pack [panedwindow "$w.p" -ori vert]
	"$w.p" add [frame "$w.t"] -stretch never
	pack [label "$w.t.l" -text "Phone" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	"$w.p" add [frame "$w.e"] -stretch never
	pack [entry "$w.e.h" -textvariable ::e_audio_host -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -width 24 ] -fill both -side left 
	pack [entry "$w.e.p" -textvariable ::e_audio_port -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -width 24 ] -fill both -side left
	"$w.p" add [frame "$w.b"] -stretch never
	#pack [button "$w.b.d" -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "destroy $w"] -fill both -side left
	pack [button "$w.b.c" -text "call" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {audio_call $::e_audio_host $::e_audio_port}] -fill both -side right

}

proc audio_call {host port} {
	puts "audio_call"
	set p [binary format H1H1 5 0]
	rawsend $host $port [wrap $p]
	show_audio_call $host $port
}

proc show_audio_call {host port} {
	set w .w_audio_call
	if { [winfo exists $w] == 1 } {
		return
	}
	toplevel $w
	wm title $w "Calling"
	pack [panedwindow "$w.p" -ori vert]
	"$w.p" add [frame "$w.t"] -stretch never
	pack [label "$w.t.l" -text "Calling" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	"$w.p" add [frame "$w.b"] -stretch never
	pack [button "$w.b.d" -text "cancel" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "audio_end $host $port ; destroy $w"] -fill both -side left
	pack [button "$w.b.a" -text "repeat" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "audio_call $host $port ; destroy $w"] -fill both -side right
	
}

proc audio_ring {host port} {
	puts "audio_ring"
	if { $::audio_state != 1 && $::audio_state != 0 } {
		puts "state not 0 and not 1"
		return
	}
	if { [winfo exists .w_audio_ring] == 0 } {
		show_audio_ring $host $port
	}
	set ::audio_state 1
	puts "ringing..."
	catch {after cancel $::audio_schedule}
	set ::audio_schedule [after 3000 [list audio_ring $host $port]]
}

proc show_audio_ring {host port} {
	set w .w_audio_ring
	if { [winfo exists $w] == 1 } {
		return
	}
	toplevel $w
	wm title $w "Ringing"
	pack [panedwindow "$w.p" -ori vert]
	"$w.p" add [frame "$w.t"] -stretch never
	pack [label "$w.t.l" -text "Ringing" -highlightcolor {#909090} -highlightbackground {#606060} -font $::font ] -fill both -side left
	"$w.p" add [frame "$w.b"] -stretch never
	pack [button "$w.b.d" -text "decline" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "set ::audio_state 0 ; destroy $w"] -fill both -side left
	pack [button "$w.b.a" -text "start" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "audio_accept $host $port ; destroy $w"] -fill both -side right
}

proc audio_accept {host port} {
	puts "audio_accept"
	set p [binary format H1H1 5 1]
	rawsend $host $port [wrap $p]
	after 1 [list audio_start $host $port]
}

proc audio_end {host port} {
	puts "audio_end"
	if { $host != $::audio_host || $port != $::audio_port } {
		puts "host not audio_host or port not audio_port"
		return
	}
	if { $::audio_state != 2 } {
		puts "state not 2"
		return
	}
	set p [binary format H1H1 5 2]
	rawsend $::audio_host $::audio_port [wrap $p]
	so destroy
	si destroy
	close $::audio_out_fifo
	close $::audio_in_fifo
	set ::audio_out_fifo {}
	set ::audio_in_fifo {}
	set ::audio_host {}
	set ::audio_port {}
	set ::audio_state {}
	set ::audio_schedule {}
}

proc udp_start {host port msg} {
	#puts "udp_start $host $port $msg"
	set msg [wrap [zip -mode compress -level default $msg]]
	set id [expr "([clock microseconds]+[string length $msg])%65536"]
	set ::udp_out_fifo($id,host) $host
	set ::udp_out_fifo($id,port) $port
	set ::udp_out_fifo($id,fifo) [fifo]
	fconfigure $::udp_out_fifo($id,fifo) -translation binary -encoding binary
	puts -nonewline $::udp_out_fifo($id,fifo) $msg
	flush $::udp_out_fifo($id,fifo)
	set all [expr "[string length $msg]/512+1"]
	set p [binary format H1su1su1su1 1 $id 0 $all]
	set p [wrap $p]
	rawsend $::udp_out_fifo($id,host) $::udp_out_fifo($id,port) $p
	after 100 [list udp_run $id 0 $all]
}

proc udp_run {id seq all} {
	#puts "udp_run $id $seq $all"
	if { [llength [array names ::udp_out_fifo "$id,fifo"]] == 0 } {
		return
	}
	set piece [read $::udp_out_fifo($id,fifo) 512]
	if { $seq >= $all } {
		after 100 [list udp_end $id $all]
	} else {	
		set p [binary format H1su1su1su1a* 2 $id $seq $all $piece]
		set p [wrap $p]
		rawsend $::udp_out_fifo($id,host) $::udp_out_fifo($id,port) $p
		after 20 [list udp_run $id [expr {$seq+1}] $all]
	}
}

proc udp_end {id all} {
	#puts "udp_end $id 0 $all"
	set p [binary format H1su1su1su1 3 $id 0 $all]
	set p [wrap $p]
	rawsend $::udp_out_fifo($id,host) $::udp_out_fifo($id,port) $p
	after 1000 [list rawsend $::udp_out_fifo($id,host) $::udp_out_fifo($id,port) $p]
	after 3000 [list rawsend $::udp_out_fifo($id,host) $::udp_out_fifo($id,port) $p]
	close $::udp_out_fifo($id,fifo)
	array unset ::udp_out_fifo "$id*"
}

proc udp_req {host port msg} {
	#puts "udp_req $host $port $msg"
	set msg [unwrap $msg]
	binary scan $msg H1 type
	#puts "udp_req type $type"
	set dmsg $msg
	binary scan $dmsg H* debug
	#puts "udp_req debug $debug"
	set id {}
	if { $type == 0 } {
		binary scan $msg a* body
		str_req "$host $port" $body 0
	} elseif { $type == 1 } { 
		binary scan $msg H1su1su1su1 type id seq all
		if { [lindex [array get ::udp_in_fifo "$id,top"] end] > 0 } {
			return
		}
		#puts "udp_req $id $seq $all"
		set ::udp_in_fifo($id,host) $host
		set ::udp_in_fifo($id,port) $port
		set ::udp_in_fifo($id,fifo) [fifo]
		set ::udp_in_fifo($id,rec) 0
		set ::udp_in_fifo($id,top) 0
		set ::udp_in_fifo($id,buf) 0
		set ::udp_in_fifo($id,all) $all
	} elseif { $type == 2 } {
		binary scan $msg H1su1su1su1a* type id seq all data
		if { $id == "" } {	
			return
		}
		#puts "udp_req $id $seq $all"
		incr ::udp_in_fifo($id,rec) 1
		#puts "udp_req $id top $::udp_in_fifo($id,top) and seq $seq"
		puts "udp_req $id in buf are: [lsort [array names ::udp_in_fifo $id,buf,*]]"
		set ::udp_in_fifo($id,buf,$seq) $data
		if { $::udp_in_fifo($id,top) == $seq } {
			return
		}
		for { set c $::udp_in_fifo($id,top) } { $c <= $seq } { incr c 1 } {
			if { [llength [array get ::udp_in_fifo $id,buf,$c]] != 0 } {
				puts "udp_req fixing missed packets $c"
				incr ::udp_in_fifo($id,top) 1
				puts -nonewline $::udp_in_fifo($id,fifo) $::udp_in_fifo($id,buf,$c)
				array unset ::udp_in_fifo "$id,buf,$c" 
			} else {
				break
			}
		}
		flush $::udp_in_fifo($id,fifo)
		return
	} elseif { $type == 3 } {
		binary scan $msg H1su1su1su1 type id seq all
		if { [llength [array names ::udp_in_fifo "$id*"]] == 0 || $id == "" } {
			return
		}
		#puts "udp_req $id $seq $all"
		puts "udp_req $id rec $::udp_in_fifo($id,rec) vs all $all"	
		#puts "udp_req $id else if fifo existing and not done"
		if { $::udp_in_fifo($id,rec) == $all } {
			#puts "udp_req $id rec $::udp_in_fifo($id,rec) == all $all"	
			set full [read $::udp_in_fifo($id,fifo)]
			str_req "$host $port" [zip -mode decompress [unwrap $full]] 0
			close $::udp_in_fifo($id,fifo)
			array unset ::udp_in_fifo "$id*"
		} else {
			#puts "udp_req $id fifo existing and not done"
			after 1000 [list udp_req $host $port $msg]
		}
	} elseif { $type == 4 } {
		puts "audio data in"
		binary scan $msg H1a* type data
		if { $::audio_in_fifo != "" } {
			puts -nonewline $::audio_in_fifo $data
			si play -blocking 0
		}
	} elseif { $type == 5 } {
		puts "audio msg in"
		binary scan $msg H1H1 type op
		if { $op == 0 } {
			# if called, ring
			puts "audio called, ring"
			audio_ring $host $port
		} elseif { $op == 1 } {
			# if accepted, start
			puts "audio accepted, start"
			audio_start $host $port
		} elseif { $op == 2 } {
			# if teardown, do
			puts "audio teardown, end"
			audio_end $host $port
		}
	} else {
		binary scan $msg H* fail
		puts "udp_req weird message $fail"
	}
}

proc send {host port msg} {
	#udp_start $host $port $msg
	after idle [list tcp_send $host $port $msg]
}

proc rawsend {host port msg} {
	#puts "send"
	if { $host == "" || $port == "" || $msg == "" } {
		puts "ERR something empty (h $host p $port), won't send $msg"
		return
	}
	if { ($host == "127.0.0.1" || $host == "localhost") && $port == $::myport } {
		puts "ERR won't send to myself"
		return
	}
	#set start [clock microseconds]
	set s [udp_open $::myport reuse]
	#puts "sending to $host $port msg $msg"
	#fconfigure $s -buffering line -remote [list $host $port]
	fconfigure $s -translation binary -encoding binary -remote [list $host $port]
	#puts "crc_wrap: [crc_wrap $msg]"
	#puts $s [crc_wrap $msg]
	puts $s $msg
	#flush $s
	close $s	
	#set end [clock microseconds]
	#puts "time send [expr {$end-$start}]"
}

proc tcp_send {host port msg} {
	puts "tcp_send"
	if { $host == "" || $port == "" || $msg == "" } {
		puts "ERR something empty (h $host p $port), won't send"
		return
	}
	if { ($host == "127.0.0.1" || $host == "localhost") && $port == $::myport } {
		puts "ERR won't send to myself"
		return
	}
	if { [clock add $::tick 1 minute] < [clock seconds] } {
		return
	}
	if { [lindex [array get ::tcp_fail count] 1] > 30 } {
		incr ::tcp_fail(count) -1
		return
	} elseif { [lindex [array get ::tcp_fail "$host,$port"] 1] > [expr "[clock seconds]-15"] } {
		return
	} else {
		array unset ::tcp_fail "$host,$port"
	}
	
	set start [clock microseconds]
	set result {}
	set s {}
	catch {
	#puts "SENDING to $host $port msg $msg"
	#if { [array get ::tcp "s,$host,$port"] == "" } { 
	#	tcp_start $host $port
	#}
	#array set ::tcp [list "last,$host,$port" [clock seconds]]
	#set s [lindex [array get ::tcp "s,$host,$port"] 1]
	#after 15000 [list tcp_check $host $port]
	set s [socket $host $port]
	} result
	if { $s == "" } {
		puts "ERR failed to open socket"
		array set ::tcp_fail [list "$host,$port" [clock seconds]]
		incr ::tcp_fail(count) 1
		return
	}
	puts "tcp_send result: $result"
	array set ::tcp [list "socket,$s" [clock seconds]]
	puts "TCP SOCKETS [llength [array names ::tcp]]"
	fconfigure $s -buffering line -translation binary
	#puts "crc_wrap: [crc_wrap $msg]"
	#puts $s "$::myport\n[crc_wrap $msg]"
	puts $s "$::myport"
	#puts "CLIENT START $host $port"
	#fconfigure $s -buffering line
	fconfigure $s -blocking 0 -buffering line
	set sid [after 1500 [list tcp_send_timeout $s]]
	fileevent $s readable [list tcp_send_res $s "$host $port" $sid ]
	#puts "CLIENT FIRST -> [string range $msg 0 31]"
	catch {
	puts $s "[crc_wrap $msg]"
	}
	#puts "CLIENT FIRST DONE"
	#set end [clock microseconds]
	#puts "time send [expr {$end-$start}]"
}

proc tcp_send_timeout {s} {
	catch { close $s }
	array unset ::tcp "socket,$s"
	puts "TCP SOCKETS [llength [array names ::tcp]]"
}

proc tcp_send_res {s peer sid} {
	catch { after cancel $sid }
	catch {
	gets $s res
	}
	#puts "CLIENT RES $res"
	if { [eof $s] } {
		#puts "CLIENT EOF CLOSE"
		close $s
		array unset ::tcp "socket,$s"
		puts "TCP SOCKETS [llength [array names ::tcp]]"
		#puts "CLIENT CLOSE DONE"
	} elseif { ![fblocked $s] } {
		#puts "CLIENT <- [string range $res 0 31]"
		lappend rs {*}[str_req $peer [crc_unwrap $res] 1]
		#puts "CLIENT RS [string range $rs 0 63]"
		foreach r $rs {
			#puts "CLIENT -> [string range $r 0 31]"
			catch {
			puts $s "[crc_wrap $r]"
			}
			#puts "CLIENT DONE"
		}
	} else {
		#puts "CLIENT BLOCKED"
	}
	#puts "CLIENT RES DONE"
}

# listen

proc crc_unwrap {fmsg} {
	#set start [clock microseconds]
	set rsum [lindex $fmsg 0]
	set msg [lrange $fmsg 1 end]
	set sum [crc::crc32 -format %X $msg]
	#set end [clock microseconds]
	#puts "time crc_unwrap [expr {$end-$start}]"
	#puts "crc_unwrap: $fmsg"
	#puts "crc_unwrap $sum vs $rsum"
	if { $sum != $rsum} {
		puts "crc $sum != $rsum"
		return 0
	} else {
		return $msg
	}
}

proc listen_handler {c} {
	set start [clock microseconds]
	set packet [read $c]
	set peer [fconfigure $c -peer]
	#puts "INCOMING: $peer / ([string length $packet]) {$packet}"
	if { $packet == {} } {
		puts "empty packet"
		return
	}
	if { $packet != 0 } {
		#set start [clock microseconds]
		#str_req $peer [crc_unwrap $packet] 0
		udp_req [lindex $peer 0] [lindex $peer 1] $packet
		#set end [clock microseconds]
		#puts "time str_req [expr {$end-$start}]"
	} else {
		puts "crc wrong, won't process"
	}
}

proc tcp_listen_handler {c host port} {
	set start [clock microseconds]
	#set fullpacket [read $c]
	#set outport [lindex [split $fullpacket "\n"] 0]
	#set packet [lindex [split $fullpacket "\n"] 1] 
	gets $c outport
	set peer "$host $outport"
	#puts "INCOMING: $peer / ([string length $packet]) {$packet}"
	#if { $packet == {} } {
	#	puts "empty packet"
	#	return
	#}
	if { $outport == {} } {
		puts "nothing good"
		return
	}
	#puts "SERVER START $peer"
	#fconfigure $c -buffering line
	array unset ::tcp_fail "$host,$port"
	array set ::tcp [list "socket,$c" [clock seconds]]
	puts "TCP SOCKETS [llength [array names ::tcp]]"
	fconfigure $c -blocking 0 -buffering line
	set sid [after 1500 [list tcp_listen_handler_timeout $c]]
	fileevent $c readable [list tcp_listen_handler_cmd $c $peer $sid]
}

proc tcp_listen_handler_timeout {c} {
	catch { close $c }
	array unset ::tcp "socket,$c"
	puts "TCP SOCKETS [llength [array names ::tcp]]"
}

proc tcp_listen_handler_cmd {c peer sid} {
	catch { after cancel $sid }
	catch {
	gets $c cmd
	}
	#puts "SERVER CMD $cmd"
	if { [eof $c] } {
		#puts "SERVER EOF CLOSE"
		close $c
		array unset ::tcp "socket,$c"
		puts "TCP SOCKETS [llength [array names ::tcp]]"
		#puts "SERVER EOF CLOSE DONE"
	} elseif { ![fblocked $c] } {
		set rs {}
		#puts "SERVER <- [string range [crc_unwrap $cmd] 0 31]"
		catch {
		lappend rs {*}[str_req $peer [crc_unwrap $cmd] 1]
		} result
		#puts "SERVER CATCH $result"
		#puts "SERVER RS [string range $rs 0 63]"
		#puts "SERVER SEND"
		foreach r $rs {
			#puts "SERVER -> [string range $r 0 31]"
			catch {
			puts $c [crc_wrap $r]
			}
			#puts "SERVER DONE"
		}
	} else {
		#puts "SERVER BLOCKED"
	}
	#puts "SERVER CMD DONE"
}

proc listen {} {
	set c [udp_open $::myport reuse]
	#fconfigure $c -buffering line -translation binary
	fconfigure $c -encoding binary -translation binary
	fileevent $c readable [list ::listen_handler $c]
	return $c	
}

proc tcp_listen {} {
	set c [socket -server [list tcp_listen_handler] $::myport]
	fconfigure $c -buffering line -translation binary
	return $c
}

#proc media_listen {} {
#	set c [udp_open $::mymediaport reuse]
#	fconfigure $c -buffering none -translation binary
#	fileevent $c readable [list ::media_listen_handler $c]
#	return $c
#}

# buckets are an array, peer is a list

proc put_to_bucket {dist peer} {
	if { [llength [split $peer]] > 1 } {
		puts "ERR put to bucket $peer is strange"
		return
	}
	if { $peer == $::me } {
		puts "ERR can't put myself to bucket"
		return
	}
	puts "put $dist,$peer to bucket"
	array set ::b [list "$dist,$peer" [expr [clock microseconds]]]
}

proc put_to_buckets {peer} {
	put_to_bucket [dist $peer $::me] $peer
}

proc oldest_in_bucket {dist} {
	return [ lindex [ lsort -integer -stride 2 -index 1 [ array get ::b "$dist,*" ] ] end ]
}

proc remove_from_bucket {dist peer} {
	array unset ::b "$dist,$peer"
}

proc remove_from_buckets {peer} {
	if { $peer == "none" } {
		return	
	}
	remove_from_bucket [dist $peer $::me] $peer
}

proc dist {a b} {
	set a_dec [format %u [expr "0x$a * 1"]]
	set b_dec [format %u [expr "0x$b * 1"]]
	set lhex [string length $::me]
	set lbin [expr {4*$lhex}]
	#return [expr {2^$lbin+(int($a_dec) ^ int($b_dec))}]
	#puts "dist:"
	#puts [expr "int($a_dec) ^ int($b_dec)"]
	return [format %u [expr "int($a_dec) ^ int($b_dec)"]]
	#return [expr "int(($a_dec ^ $b_dec)>>32)"]
}

proc distorder {dist} {
	set lhex [string length $::me]
	set lbin [expr {4*$lhex}]
	for {set c 0} {$c < $lbin} {incr c} {
		if { [expr {2^($c+1)}] > $dist} {
			return $c	
		}
	}
	return -1
}

proc closest_in_buckets {key num} {
	#puts "find $num closest keys in buckets to"
	#puts "  key $key"
	if { [llength [split $key]] > 1 } {
		puts "ERR $key is strange"
	}
	set min [dist $key $::me]
	#puts "distance between me and $key is $min"
	array set xorred {}
	for {set c 0} {$c < $num} {incr c} {
		foreach bkey [array names ::b] {
			set sbkey [lindex [split $bkey {,}] 1]
			#puts "bkey is $bkey, sbkey is $sbkey"
			#puts "dist is [dist $key $sbkey]"
			#array set xorred [list $sbkey [dist $key $sbkey]]
			array set xorred [list [dist $key $sbkey] $sbkey]
		}
	}
	#puts "closest - xorred array: [array get xorred]"
	#set ret {}
	#foreach {pkey ptime} [lrange [lsort -integer -stride 2 -index 1 [array get xorred]] 0 $num] {
	#	foreach {pskey psvalue} [array get ::peerstore "$pkey*"] {
	#		lappend ret $psvalue
	#	}
	#}
	set ret {}
	foreach {ptime pkey} [lsort -integer -stride 2 -index 0 [array get xorred]] {
		#puts "closest ptime $ptime pkey $pkey" 
		foreach {pskey psvalue} [array get ::peerstore "$pkey*"] {
			#puts "closest pskey $pskey psvalue $psvalue"
			lappend ret $psvalue
		}
	}
	set ret [lrange $ret 0 $num]
	#puts "returning closest: $ret"
	#puts "returning closest"
	#puts $ret
	return $ret
}

# cmds

proc sol {host port} {
	send $host $port "SOL $::me"
}

proc ping {s host port} {
	send $host $port "PING $s"
	after 30000 [list str_ping $s "TIMEOUT" 0]
}

proc store {s host port key val} {
	send $host $port "STORE $s $key $val"
	after 30000 [list str_store $s "TIMEOUT"]
}

proc find_node {s host port key} {
	send $host $port "FIND_NODE $s $key"
	after 30000 [list str_find_node $s "TIMEOUT" 0]
}

proc find_value {s host port key} {
	send $host $port "FIND_VALUE $s $key"
	after 30000 [list str_find_value $s "TIMEOUT" 0]
}

# strategies - process is a state START REQ DONE FAIL

proc str_create {req host port key value} {
	if { $host == "" || $port == "" } {
		return
	}
	if { ($host == "127.0.0.1" || $host == "localhost") && $port == $::myport } {
		return
	}
	set s [clock microseconds]
	if { $req == "PING" } {
		array set ::p [list "$s,req" "$req" "$s,host" "$host" "$s,port" "$port" "$s,key" "$key" "$s,value" "$value" "$s,state" "START" "$s,ttl" "6"]
	} elseif { $req == "STORE" } {
		array set ::p [list "$s,req" "$req" "$s,host" "$host" "$s,port" "$port" "$s,key" "$key" "$s,value" "$value" "$s,state" "START" "$s,ttl" "6"]
	} elseif { $req == "FIND_NODE" } {
		array set ::p [list "$s,req" "$req" "$s,host" "$host" "$s,port" "$port" "$s,key" "$key" "$s,value" "$value" "$s,state" "START" "$s,ttl" "6"]
	} elseif { $req == "FIND_VALUE" } {
		array set ::p [list "$s,req" "$req" "$s,host" "$host" "$s,port" "$port" "$s,key" "$key" "$s,value" "$value" "$s,state" "START" "$s,ttl" "6"]
	} elseif { $req == "ASK" } {
		array set ::p [list "$s,req" "$req" "$s,host" "$host" "$s,port" "$port" "$s,key" "$key" "$s,value" "$value" "$s,state" "START" "$s,ttl" "6"]
	} else {
		array set ::p [list "$s,req" "$req" "$s,host" "$host" "$s,port" "$port" "$s,key" "$key" "$s,value" "$value" "$s,state" "ERROR" "$s,ttl" "6"]
		puts "Unknown"
	}
	return $s
}

proc str_start {s} {
	if { $s == "" } {
		return
	}
	if { $::p($s,state) != "START" } {
		puts "can't START in state: "
		puts $::p($s,state)
		return -1
	}
	if { $::p($s,req) == "PING" } {
		ping $s $::p($s,host) $::p($s,port)
		array set ::p [list "$s,state" "REQ" "$s,start" [clock microseconds] "$s,change" [clock microseconds]]
	} elseif { $::p($s,req) == "STORE" } {
		store $s $::p($s,host) $::p($s,port) $::p($s,key) $::p($s,value)
		array set ::p [list "$s,state" "REQ" "$s,start" [clock microseconds] "$s,change" [clock microseconds]]
	} elseif { $::p($s,req) == "FIND_NODE" } {
		find_node $s $::p($s,host) $::p($s,port) $::p($s,key)
		array set ::p [list "$s,state" "REQ" "$s,start" [clock microseconds] "$s,change" [clock microseconds]]
	} elseif { $::p($s,req) == "FIND_VALUE" } {
		find_value $s $::p($s,host) $::p($s,port) $::p($s,key)
		array set ::p [list "$s,state" "REQ" "$s,start" [clock microseconds] "$s,change" [clock microseconds]]
	} elseif { $::p($s,req) == "ASK" } {
		ask $s $::p($s,host) $::p($s,port) $::p($s,key) $::p($s,value)
		array set ::p [list "$s,state" "REQ" "$s,start" [clock microseconds] "$s,change" [clock microseconds]]
	} else {
		array set ::p [list "$s,state" "ERROR" "$s,start" [clock microseconds] "$s,change" [clock microseconds]]
		puts "Unknown"	
	}
}

proc str_req {f p tcp} {	
	#puts "str_req $f $p"
	#puts "STR_REQ $p"
	set m [lindex $p 0]
	set s [lindex $p 1]
	set r [lindex $p 2]
	set a [lrange $p 3 end]
	set start [clock microseconds]
	set sr {}
	if { $m == "PING" } {
		send [lindex [split $f { }] 0] [lindex [split $f { }] 1] [list "RES" $s "OK" $::me]
		return
	} elseif { $m == "STORE" } {
		#puts "store value r=$r a=$a"
		array set ::valuestore [list "$r" $a]
		array set ::valuestore [list $r,[expr "[clock microseconds]%16"] $a]
		send [lindex [split $f { }] 0] [lindex [split $f { }] 1] [list "RES" $s "OK"]
		after 200 check_waitvalues
		return
	} elseif { $m == "FIND_NODE" } {
		set res {}
		foreach {key value} [array get ::peerstore "$r*"] {
			if { $value == "" } {
					continue
			}
			lappend res $value
		}
		#puts "sortedres [lrange [lsort -decreasing $res] 0 8]"
		set lenres [llength $res]
		if { $lenres > 7 } {
			set $lenres 7
		}
		if { $lenres > 0 } {
			puts "FIND_NODE"
			set sres [lrange [lsort -decreasing $res] 0 8]
			send [lindex [split $f { }] 0] [lindex [split $f { }] 1] [list "RES" $s "OK" [lindex $sres [expr "[clock microseconds]%[llength $sres]"]] ]
			#puts [list "RES" $s "OK" [lindex [array get ::peerstore "$r"] 1]]
			#send [lindex [split $f { }] 0] [lindex [split $f { }] 1] [list "RES" $s "OK" [lindex [array get ::peerstore "$r"] 1]]
		} else {
			puts "FIND_NODE"
			#puts [list "RES" $s "PEERS" [closest_in_buckets $r 4]]
			send [lindex [split $f { }] 0] [lindex [split $f { }] 1] [list "RES" $s "PEERS" [closest_in_buckets $r 4]]
		}
		return
	} elseif { $m == "FIND_VALUE" } {
		set stores [ list ::contacts ::groups ::headers ::sources]
		set res {}
		foreach store $stores {
			foreach {key value} [array get $store "$r*"] {
				if { $value == "" } {
					continue
				}
				lappend res $value
			}
		}
		#puts "sortedres [lrange [lsort -decreasing $res] 0 8]"
		set lenres [llength $res]
		if { $lenres > 8 } {
			set $lenres 8
		}
		if { $lenres > 0 } {
			puts "FIND VALUE"
			set sres [lrange [lsort -decreasing $res] 0 7]
			#send [lindex [split $f { }] 0] [lindex [split $f { }] 1] [list "RES" $s "OK" "$sres" ]
			#foreach val $sres {
			#	send [lindex [split $f { }] 0] [lindex [split $f { }] 1] [list "RES" $s "OK" "$val" ]
			#}
			puts "findval send res $sres"
			send [lindex [split $f { }] 0] [lindex [split $f { }] 1] [list "RES" $s "OK" [lindex $sres [expr "[clock microseconds]%[llength $sres]"]]]
			#puts [list "RES" $s "OK" [lindex [array get ::valuestore "$r"] 1]]
			#send [lindex [split $f { }] 0] [lindex [split $f { }] 1] [list "RES" $s "OK" [lindex [array get ::valuestore "$r"] 1]]
		} else {
			puts "not found value, sending "
			puts "FIND_VALUE"
			#puts [list "findval peers" "RES" $s "PEERS" {*}[closest_in_buckets $r 4]]
			send [lindex [split $f { }] 0] [lindex [split $f { }] 1] [list "RES" $s "PEERS" {*}[closest_in_buckets $r 4]]
		}
		return
	} elseif { $m == "SOL" } {
		array set ::peerstore [list $s "$s:[lindex [split $f { }] 0]:[lindex [split $f { }] 1]"]
		puts "add to peerstore"
		puts [list $s "$s:[lindex [split $f { }] 0]:[lindex [split $f { }] 1]"]
		# commented, because check_peers does that
		# uncommented because it doesn't
		after 200 check_peers
		#put_to_buckets $s
		# set fields
		puts [list $s "$s:[lindex [split $f { }] 0]:[lindex [split $f { }] 1]"]
		set ::formhost [lindex [split $f { }] 0]
		set ::formport [lindex [split $f { }] 1] 
		set ::formkey $s
		return
	} elseif { $m == "CHAT" } {
		chat_recv [lindex [split $f { }] 0] [lindex [split $f { }] 1] $r
		return
	} elseif { $m == "GCHAT" } {
		gchat_recv [lindex [split $f { }] 0] [lindex [split $f { }] 1] $r
		return
	} elseif { $m == "GENC" } {
		set dmsg {}
		catch {
		set dmsg [dec_msg $r]
		}
		if { $dmsg == "" } {
			puts "failed to decrypt"
			#set type "RENEW"
			#set body "ask"
			#set id [clock microseconds]
			#set msg [wrap "$gid $sid $id [clock seconds] $type $body"]
			#ml_genc {} [lindex [split $f { }] 0] [lindex [split $f { }] 1] $msg 0
			return
		}
		#puts "GENC f $f r $r a $a dmsg $dmsg"
		set sr [str_req $f [string trim $dmsg] $tcp]
		#puts "SR $sr"
		return $sr
	} elseif { $m == "GET" } {
		#puts "plain $m $s $r $a"
		if { $s != 0 } {	
			puts "plain GET $s answer $r"
			return
		}
		set piece [dl_read $r]
		if { $piece != "" } { 
			send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "GET 1 OK"
			#tcp_send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "GIVE 0 $piece"
			send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "GIVE 0 $piece"
		} else {
			send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "GET 1 FAIL"
		}
		return
	} elseif { $m == "GIVE" } {
		#puts "plain $m $s $r $a"
		if { $s != 0 } {	
			puts "plain GIVE $s answer $r"
			return
		}
		set ret [dl_write $r]
		if { $ret == 0 } {
			send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "GIVE 1 OK"
		} else {
			send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "GIVE 1 FAIL"
		}
		return
	} elseif { $m == "HEAD" } {
		#puts "got HEAD s $s r $r a $a"
		if { $s != 0 } {
			puts "plain HEAD $s answer $r"
			return
		}

		if { $r == "PWT" } {
			set sc [ml_screen p [lindex $a 0] [lindex [split $f { }] 0] [lindex [split $f { }] 1]]
		} elseif { $r == "DIG" } {
			set sc [ml_screen g [lindex $a 0] [lindex [split $f { }] 0] [lindex [split $f { }] 1]]
		} else {
			set sc 0
		}
		if { $sc != 0 } {
			puts "screened out $f"
			return
		}

		set ret {}
		if { $r == "CNT" } {
			set ret [lrange [ml_get_hdrs $a] start end-1]
		} elseif { $r == "SRC" } {
			#if { $::group_host_mode == 1 && [array get ::jgroups $a] != "" } {
			#	set ret [dict get [ml_groupdict [lindex [array get ::jgroups $a] end]] peerid]
			#} elseif { $::group_host_mode != 1 }  {
			#	set ret [ml_get_srcs $a]
			#} else {
			#	set ret [list $a 0 {}]	
			#}
			# commented, because it matters where I send, not what I know
			set ret [ml_get_srcs $a]
			puts "ml_get_srcs $a -> $ret"
		} elseif { $r == "SIG" } {
			set ret [array get ::group_to_sig "$a,*"]
			puts "ml sig $a -> $ret"
		} elseif { $r == "DIG" } {
			set ret [ml_get_hdrs $a]
			puts "ml_get_hdrs $a -> $ret"
		} elseif { $r == "EML" } {
			set ret {}
			foreach hash $a {
				set ml [wrap [ml_get_eml $hash]]
				if { $ml == "" || $ml == -1 } {	
					continue
				}
				lappend ret $ml 
			}
		} elseif { $r == "PWT" } {
			set ret [ml_get_personhdrs $a]
		} elseif { $r == "GSH" } {
			set ret [list g $a [wrap [array get ::group_file_by_hash]]]
		} elseif { $r == "PSH" } {
			set ret [list p $::me [wrap [array get ::file_by_hash]]]
		} else {
			return
		}
		if { [llength $ret] > 0 } {
			#puts "HEAD ret > 0"
			#send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "HEAD 1 OK"
			lappend sr [ml_genc {} [lindex [split $f { }] 0] [lindex [split $f { }] 1] "HEAD 1 OK" $tcp]
			#tcp_send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "MAIL 0 $r $ret"
			#send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "MAIL 0 $r $ret"
			lappend sr [ml_genc {} [lindex [split $f { }] 0] [lindex [split $f { }] 1] "MAIL 0 $r $ret" $tcp]
			#ml_genc {} [lindex [split $f { }] 0] [lindex [split $f { }] 1] "MAIL 0 $r $ret" 0
		} else {
			#puts "HEAD ret == 0"
			#send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "HEAD 1 FAIL"
			lappend sr [ml_genc {} [lindex [split $f { }] 0] [lindex [split $f { }] 1] "HEAD 1 FAIL" $tcp]
		}
		#puts "SR $sr"
		return $sr
	} elseif { $m == "MAIL" } {	
		#puts "got MAIL s $s r $r a $a"
		if { $s != 0 } {
			puts "plain MAIL $s answer $r"
			return
		}

		if { $r != "EML" && $r != "SIG" && $r == "PWT" } {
			set sc [ml_screen p [lindex $a 0] [lindex [split $f { }] 0] [lindex [split $f { }] 1]]
		} elseif { $r != "EML" && $r != "SIG" && $r != "PWT" } {
			set sc [ml_screen g [lindex $a 0] [lindex [split $f { }] 0] [lindex [split $f { }] 1]]
		} else {
			set sc 0
		}
		if { $sc != 0 } {
			puts "screened out $f"
			return
		}

		if { $r == "CNT" } {
			set ::ml_cnt_grp [list [lindex $a 0],[clock seconds] [lindex $a 1]]
		} elseif { $r == "SRC" } {
			set g [lindex $a 0]
			set n [lindex $a 1]
			set srcs [lrange $a 2 end]
			puts "SRC g $g n $n srcs $srcs"
			sc_get_contacts $srcs
			sc_get_contacts [prep_contact_keys $srcs]
			ml_add_srcs $g $srcs
			#lappend sr [ml_genc {} [lindex [split $f { }] 0] [lindex [split $f { }] 1] "MAIL 0 SRC $g 1 $::me" $tcp]
		} elseif { $r == "SIG" } {
			puts "ml get sigs"
			foreach {key val} $a {
				if { [array get ::group_to_sig $key] == "" } {
					if { [ml_check_sig g [lindex $key 0] [lindex $key 1] 1] != 0 } {
						array set ::group_to_sig [list $key $val]
					}
				}
			}
		} elseif { $r == "DIG" } {
			set g [lindex $a 0]
			set n [lindex $a 1]
			set hdrs [lrange $a 2 end]
			puts "hdrs $hdrs"
			#set myhdrs [lrange [ml_get_hdrs $g] 2 end]
			#puts "myhdrs $myhdrs"
			#set nhdrs [lmap n $hdrs {
			#	if { $n in $myhdrs } {
			#		continue
			#	}
			#	set	n
			#}] 
			#puts "nhdrs $nhdrs"
			#ml_add_hdrs $g $nhdrs
			ml_add_hdrs $g $hdrs
			set hashes {}
			foreach hdr $hdrs {
				set h [header_to_dict $hdr]
				if { $h == "" } {
					continue
				}
				set hash [dict get $h hash]
				if { [file exists [file join $::filepath mailnews $hash]] == 0 } { 
					lappend hashes $hash
				}
			}
			set hashes [ml_filter_del $hashes]
			puts "send HEAD 0 EML $hashes"
			#send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "HEAD 0 EML $hashes"
			#lappend sr [ml_genc {} [lindex [split $f { }] 0] [lindex [split $f { }] 1] "HEAD 0 EML $hashes" $tcp]
			ml_genc {} [lindex [split $f { }] 0] [lindex [split $f { }] 1] "HEAD 0 EML $hashes" 0
			after 1000 ml_showlist $g
		} elseif { $r == "EML" } {
			foreach eml $a {
				ml_add_eml [unwrap $eml]
			}
		} elseif { $r == "PWT" } {
			set p [lindex $a 0]
			set n [lindex $a 1]
			set hdrs [lrange $a 2 end]
			ml_add_personhdrs $p $hdrs
			set hashes {}
			foreach hdr $hdrs {
				set h [header_to_dict $hdr]
				if { $h == "" } {
					continue
				}
				set hash [dict get $h hash]
				if { [file exists [file join $::filepath mailnews $hash]] == 0 } { 
					lappend hashes $hash
				}
			}
			set hashes [ml_filter_del $hashes]
			#puts "send HEAD 0 EML $hashes"
			#send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "HEAD 0 EML $hashes"
			#lappend sr [ml_genc $p [lindex [split $f { }] 0] [lindex [split $f { }] 1] "HEAD 0 EML $hashes" $tcp]
			ml_genc $p [lindex [split $f { }] 0] [lindex [split $f { }] 1] "HEAD 0 EML $hashes" 0
			after 1000 ml_showpersonlist $p
		} elseif { $r == "GSH" } {
			show_files {*}$a
			return
		} elseif { $r == "PSH" } {
			show_files {*}$a
			return
		} else {
			#send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "MAIL 1 FAIL"
			lappend sr [ml_genc {} [lindex [split $f { }] 0] [lindex [split $f { }] 1] "MAIL 1 FAIL" $tcp]
			#puts "SR $sr"
			return $sr
		}
		#send [lindex [split $f { }] 0] [lindex [split $f { }] 1] "MAIL 1 OK"
		lappend sr [ml_genc {} [lindex [split $f { }] 0] [lindex [split $f { }] 1] "MAIL 1 OK" $tcp]
		#puts "SR $sr"
		return $sr
	}

	if { [llength [array get ::p "$s*"]] == 0 } {
		puts "no session str_req f:$f p:$p host:[lindex [split $f { }] 0] port:[lindex [split $f { }] 1] m:$m s:$s r:$r a:$a"
		puts "no session"
		return -1
	} 	
	if { $s == "" } {
		puts "no s, full message is $f $p"
		return
	}

	if { $::p($s,state) != "REQ" } {
		puts "can't do REQ in state: "
		puts $::p($s,state)
		return -1
	}
	
	set ttl $::p($s,ttl)
	if { $ttl <= 0 } {
		array set ::p [list "$s,state" "DONE" "$s,change" [clock microseconds]]
		return
	} else {
		array set ::ttl [list "$s,ttl" [expr "$ttl-1"]]
	}

	puts "strategy $::p($s,req)"
	if { $::p($s,req) == "PING" } {
		str_ping $s $r $a
	} elseif { $::p($s,req) == "STORE" } {
		str_store $s $r
	} elseif { $::p($s,req) == "FIND_NODE" } {
		str_find_node $s $r $a
	} elseif { $::p($s,req) == "FIND_VALUE" } {
		str_find_value $s $r $a
	} else {
		puts "Unknown"
	}	
}

proc str_ping {s r a} {
	if { $r == "OK" } { 
		puts "PING $s $r $a"
		array set ::p [list "$s,state" "DONE" "$s,change" [clock microseconds]]
		if { $a != $::me && $a != "" } {
			set peer "$a:$::p($s,host):$::p($s,port)"
			puts "put to peerstore [list $a $peer]"
			array set ::peerstore [list $a $peer]
			sol $::p($s,host) $::p($s,port)
			put_to_buckets $a
		}
	} else {
		if { [lindex [array get ::p "$s,state" ] 1] == "DONE" } {
			puts "already done"
			return
		}
		puts "PING $s $r FAIL"
		array set ::p [list "$s,state" "FAIL" "$s,change" [clock microseconds]]
		remove_from_buckets $::p($s,key)
	}
	after 200 check_peers
}

proc str_store {s r} {
	if { $r == "OK" } {
		array set ::p [list "$s,state" "DONE" "$s,change" [clock microseconds]]
		#puts "$s stored $r"
		after 10000 [list array unset ::p "$s,*"]
	}	else {
		if { [lindex [array get ::p "$s,state" ] 1] == "DONE" } {
			return
		}
		array set ::p [list "$s,state" "FAIL" "$s,change" [clock microseconds]]
	}
}

proc str_find_node {s r a} {
	if { $r == "PEERS" && $a != {} } {
		#puts "findnode peers $a"
		foreach item [split [concat $a]] {
			#puts "findnode peer $item"
			set peer [split $item {:}]
			array set ::peerstore [list [lindex $peer 0] $item]
			find_node $s [lindex $peer 1] [lindex $peer 2] $::p($s,key)
		}
		array set ::p [list "$s,change" [clock microseconds]]
	} elseif { $r == "OK" } {
		array set ::peerstore [list $::p($s,key) $a]
		array set ::p [list "$s,state" "DONE" "$s,change" [clock microseconds]]
	} else {
		if { [lindex [array get ::p "$s,state" ] 1] == "DONE" } {
			return
		}
		array set ::p [list "$s,state" "FAIL" "$s,change" [clock microseconds]]
	}
	after 200 check_peers
}

proc str_find_value {s r a} {
	if { $r == "PEERS" && $a != {} } {
		#puts "findval peers $a"
		foreach item [split [concat $a]] {
			#puts "findval peer $item"
			set peer [split $item {:}]
			array set ::peerstore [list [lindex $peer 0] $item]
			find_value $s [lindex $peer 1] [lindex $peer 2] $::p($s,key)
		}
		array set ::p [list "$s,change" [clock microseconds]]
		after 200 check_peers
	} elseif { $r == "OK" } {
		array set ::valuestore [list $::p($s,key) $a]
		array set ::valuestore [list $::p($s,key),[expr "[clock microseconds]%16"] $a]
		array set ::p [list "$s,state" "DONE" "$s,change" [clock microseconds]]
		after 200 check_waitvalues
	} else {
		if { [lindex [array get ::p "$s,state" ] 1] == "DONE" } {
			return
		}
		array set ::p [list "$s,state" "FAIL" "$s,change" [clock microseconds]]
	}
}

#
# FS - I don't need storage management - I already have a filesystem
# I need CREATE, READ, UPDATE and DELETE operations
#
# I notably need them on dirs as much as on files
# I can simply use every path as resource to be created, updated, read or deleted
# value is ctime:mtime:ownerpeerid:size:type:filehash
#   type is dir/file
#   filehash is empty for dirs
#   atime is not needed
#
# thus I flatten the FS into a simple index, storing it in a directory

proc ml_filedict {fvalue} {
	set s [split $fvalue {:}]
	set d {}
	dict set d ctime [lindex $s 0]
	dict set d mtime [lindex $s 1]
	dict set d ownerpeerid [lindex $s 2]
	dict set d size [lindex $s 3]
	dict set d type [lindex $s 4]
	dict set d filehash [lindex $s 5]
	return $d
}

proc ml_dictfile {fdict} {
	set ctime [dict get $fdict ctime]
	set mtime [dict get $fdict mtime]
	set ownerpeerid [dict get $fdict ownerpeerid]
	set size [dict get $fdict size]
	set type [dict get $fdict type]
	set filehash [dict get $fdict filehash]
	return "$ctime:$mtime:$ownerpeerid:$size:$type:$filehash"
}

###
# some kind of local (no exchange yet) group filesystem browser
### 
proc show_files args {
	if { [winfo exists .fff] == 1 } {	
		return
	}
	set t [lindex $args 0]
	set id [lindex $args 1]
	set files [unwrap [lrange $args 2 end]]
	set ::fff_pwd /
	set ::fff_cid $id
	set ::fff_files $files
	toplevel .fff
	wm title .fff "Files $t id $id files $files"
	pack [panedwindow .fff.p -ori vert] -fill both -expand 1
	.fff.p add [frame .fff.t] -stretch never 
	#pack [button .fff.t.x -text "exit" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command "destroy .fff"] -fill both -side left 
	.fff.p add [frame .fff.l] -stretch always
	pack [scrollbar .fff.l.y -activebackground {#606060}  -troughcolor {#606060}  -command "tl_yview 2 .fff.l.n .fff.l.s"] -fill y -side right
	pack [listbox .fff.l.n -listvariable ::ml_fnode_name -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 24 -width 40 -yscrollc ".fff.l.y set"] -fill both -expand 1 -side left
	pack [listbox .fff.l.s -listvariable ::ml_fnode_size -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060} -selectforeground {#000000} -selectbackground {#606060} -font $::font -height 24 -width 8 -yscrollc ".fff.l.y set"] -fill y -side left
	.fff.p add [frame .fff.b] -stretch never 
	pack [button .fff.b.get -text "get" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {show_filedialog [lindex $::ml_fnode [lindex [.fff.l.n index active] 0]] {}}] -fill both -side right
	#.fff.p add [frame .fff.n] -stretch never 
	#pack [button .fff.n.up -text "up" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {ml_schedup}] -fill both -side right
	#pack [button .fff.n.down -text "down" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {ml_scheddown}] -fill both -side right
	#.fff.p add [frame .fff.b] -stretch never 
	#pack [button .fff.b.get -text "get" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {ml_schedget [lindex $::ml_fnode_name [lindex [.fff.l.n index active] 0]]}] -fill both -side right
	#pack [button .fff.b.put -text "put" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {ml_schedput}] -fill both -side right
	#pack [button .fff.b.mkdir -text "mkdir" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {ml_schedmkdir}] -fill both -side right
	#pack [button .fff.b.del -text "delete" -activebackground {#606060} -activeforeground {#000000} -highlightthickness $::line_th -highlightcolor {#909090} -highlightbackground {#606060}  -font $::font -command {ml_scheddel [lindex $::ml_fnode_name [lindex [.fff.l.n index active] 0]]}] -fill both -side right
	update_fnodes $::fff_files
}

proc ml_schedup {} {
	if { $::fff_pwd == {/} } {
		return
	}
	set sp [file split $::fff_pwd]
	set np [file join [lrange $sp 0 end-1]]
	set ::fff_pwd $np
	update_fnodes
}

proc ml_scheddown {name} {
	if { $name == "" } {
		return
	} 
	set np [file join $::fff_pwd $name]
	set ::fff_pwd $np
	update_fnodes
}

proc ml_schedget {name} {
	set fp [file join $::fff_pwd $name]
	set fv [ml_get_file $::fff_gid $fp]
	# need to add file to downloads and index by path
	set d {}
	dict set d filename $name
	dict set d filehash [dict get $fv hash] 
	dict set d offset 0
	dict set d len [dict get $fv size] 
	dl_add [dl_dictreq $d] {} {}
}

proc ml_schedput {} {
	set path [tk_getOpenFile]
	set sname [lindex [file split $path] end]
	puts "path $path"
	set f [open $path r]
	fconfigure $f -translation binary -encoding binary
	set data [read $f]
	close $f
	ml_add_eml $data
	set hash [sha1::sha1 $data]
	set npath [file join $::fff_pwd $sname]
	# need to create structure and index by path
	set struct "[clock seconds]:[clock seconds]:$::me:[string length $data]:file:$hash"
	ml_add_file $::fff_gid $npath $struct
	update_fnodes
}

proc ml_schedmkdir {} {
	#show_mkdir_entry $::fff_gid $::fff_pwd
	# need to index directory by path
	update_fnodes
}

proc ml_scheddel {name} {
	set fp [file join $::fff_pwd $name]
	# need to remove from index by path
	update_fnodes
}

proc update_fnodes {files} {
	if { $::fff_pwd == "" } {
		set ::fff_pwd / 
	}
	set cid $::fff_cid
	set ::ml_fnode_name {}
	set ::ml_fnode_size {}
	set ::ml_fnode_hash {}
	set ::ml_fnode {}
	foreach {fcur d} $files {
		puts "dict $d"
		lappend ::ml_fnode_name [unwrap [dict get $d name]]
		lappend ::ml_fnode_size [dict get $d size]
		lappend ::ml_fnode_hash $fcur 
		lappend ::ml_fnode [dict get $d name]:$fcur:0:[dict get $d size]
	}
}

###

proc ml_showmsg {hdr} {
	.m.f.t delete 1.0 end
	if { $hdr == "" } {
		puts "no header"
		return
	}
	set h [header_to_dict $hdr]
	if { $h == "" } {
		puts "bad header"	
		return
	}

	set hash [dict get $h hash]
	set type [dict get $h type]

	if { $type == "p" && [dict get $h to] == $::me } {
		if {[file exists [file join $::filepath mailnews dec $hash]]} {
			set body [ml_get_eml "dec_$hash"]
		} else {
			set body [ml_get_eml $hash]
			set aeskey_e [unwrap [lindex [split $body {,}] 0]]
			set aeskey [::pki::decrypt -hex -priv $aeskey_e $::mekey]
			set body_e [unwrap [lindex [split $body {,}] 1]]
			set nil_block [string repeat \0 16]

			set body [string trimright [::aes::aes -mode cbc -dir decrypt -key $aeskey -iv $nil_block $body_e] \0]

			set f [open [file join $::filepath mailnews dec $hash] w]
			fconfigure $f -translation binary
			puts -nonewline $f $body
			flush $f
			close $f
		}
		set comment "personal to me"
	} elseif { $type == "p" && ([dict get $h kfrom] == [::pki::public_key $::mekey] || [dict get $h kfrom] == {}) && [file exists [file join $::filepath mailnews plain $hash]] == 1 } {
		set body [ml_get_eml "plain_$hash"]
		set comment "personal by me"
	} elseif { $type == "g" } {
		set body [ml_get_eml $hash]
		set comment "group message"
	} else {
		set body "oopsie"
		set comment "oopsie"
	}

	if { $body == -1 || $body == "" } {
		return
	}

	set body [encoding convertfrom utf-8 $body]	
	set lines [split $body "\n"]
	set from [lindex $lines 0]
	set to [lindex $lines 1]
	set subject [lindex $lines 2]
	set epoch [lindex $lines 3]
	set bodylines [lrange $lines 5 end]
	.m.f.t tag configure red -foreground {#c06060} 
	.m.f.t tag configure cyan -foreground {#6090c0}
	.m.f.t tag configure blue -foreground {#6060c0}
	.m.f.t tag configure yellow -foreground {#c09060}
	.m.f.t tag configure magenta -foreground {#c060c0}
	.m.f.t tag configure hide -elide true
	.m.f.t tag configure headerlink -foreground {#6060c0} -underline true
	.m.f.t tag configure mlfilelink -foreground {#6060c0} -underline true
	.m.f.t tag configure attachlink -foreground {#c06060} -underline true
	#.m.f.t insert end "Comment: $comment\n" {red}
	#.m.f.t insert end "   Sent: [clock format [dict get $h epoch] -format {%Y-%m-%d %H:%M:%S}]\n" {red}
	.m.f.t insert end "Comment: $comment ; [clock format [dict get $h epoch] -format {%Y-%m-%d %H:%M:%S}]\n" {red}
	.m.f.t insert end "$from\n" {cyan}
	.m.f.t insert end "$to\n" {cyan}
	.m.f.t insert end "$subject\n" {cyan}
	.m.f.t insert end "\n"
	foreach line $bodylines {
		if { [lindex [split $line { }] 0] == "FILE" } {
			set tag {blue mlfilelink}
		} elseif { [lindex [split $line { }] 0] == "HDR" } {
			.m.f.t insert end "$line " {hide headerlink} 
			set nline "[lrange $line 2 end] hash://[lindex [split [lindex $line 1] {:}] 0]\n"
			set line $nline
			set tag {headerlink}
		} elseif { [lindex [split $line { }] 0] == "ATTACH" } {
			#.m.f.t insert end "$line " {hide attachlink}
			set nline "[string trim [lindex [split $line {()}] 0]]\n"
			#set line $nline
			set line {}
			set tag {attachlink}
		} elseif { [string index $line 0] == ">" } {
			set tag {magenta}
		} else {
			set tag {} 
		}
		.m.f.t insert end "$line\n" $tag
	}
	.m.f.t insert end "\n"
}

proc ml_showpersonlist {person} {
	set ::ml_topline "person: $::ml_cur_person_l"
	set ::msglist_l {}
	set ::msglist_k {}
	puts "filling msglist"
	if { $::searchfield != "" } {
		set reg [string map {{ } {.*}} $::searchfield]
	} else {
		set reg {.*}
	}
	puts "regex is $reg"
	set sorted {}
	foreach hdr [lrange [ml_get_personhdrs $person] 2 end] {
		set h [header_to_dict $hdr]	
		if { $h == "" } {
			continue
		}
		if { [regexp $reg $h] == 0 } {
			continue	
		}
		set t "[clock format [dict get $h epoch] -format {%Y-%m-%d %H:%M:%S}] | [string range [dict get $h from] 0 3] | [string range [dict get $h nickname] 0 11] | [string range [dict get $h hash] 0 3] | [string range [dict get $h subject] 0 31] ([dict get $h len])"
		lappend sorted $t $hdr [dict get $h epoch]
	}
	foreach {title hdr epoch} [lsort -decreasing -stride 3 -index end $sorted] {
		lappend ::msglist_l $title
		lappend ::msglist_k $hdr
	}
}

# command for personal mail is PWT for headers, usual EML to ask with hashes
proc ml_personhead {contact} {
	foreach {hash con} [array get ::buddies] {
		if { $con == $contact } {
			chat_send $hash [clock microseconds] "RENEW" "ask"
		}
	}
	set c [contact_to_dict $contact]
	set peerid [dict get $c peerid]
	# find if we have this peer in store
	set foundpeers [lsort -unique -stride 2 -index end [array get ::peerstore "$peerid*"]]
	# if not, ask DHT
	if { [llength $foundpeers] == 0 } {
		set peers [lsort -unique [closest_in_buckets $peerid 4]]
		#puts "ml_personhead mail peers $peers"
		foreach peer $peers {
			set speer [split $peer {:}]
			#puts "ml_personhead asking $peer for our mail contact"
			if {[llength $speer] == 3} {
				#puts "ml_personhead $speer ask him for $peerid"
				after idle [list str_start [str_create FIND_NODE [lindex $speer 1] [lindex $speer 2] $peerid none]]
			} else {
				puts "ml_personhead $speer invalid peer"
			}
		}
	# send HEAD to each found peer 
	} else {
		foreach {key peer} $foundpeers {
			#puts "ml_personhead send key $key peer $peer"
			set speer [split $peer {:}]
			#send [lindex $speer 1] [lindex $speer 2] "HEAD 0 PWT $::me"
			after idle [list ml_genc [lindex $speer 0] [lindex $speer 1] [lindex $speer 2] "HEAD 0 PWT $::me" 0]
		}
	}
	after 200 [list ml_showpersonlist $peerid]
	#after 1000 [list ml_showpersonlist $peerid]
	#after 5000 [list ml_showpersonlist $peerid]
}

proc ml_personbrowse {contact} {
	set c [contact_to_dict $contact]
	set peerid [dict get $c peerid]
	set foundpeers [lsort -unique -stride 2 -index end [array get ::peerstore "$peerid*"]]
	if { [llength $foundpeers] == 0 } {
		set peers [lsort -unique [closest_in_buckets $peerid 4]]
		#puts "ml_personhead mail peers $peers"
		foreach peer $peers {
			set speer [split $peer {:}]
			#puts "ml_personhead asking $peer for our mail contact"
			if {[llength $speer] == 3} {
				#puts "ml_personhead $speer ask him for $peerid"
				str_start [str_create FIND_NODE [lindex $speer 1] [lindex $speer 2] $peerid none]
			} else {
				puts "ml_personhead $speer invalid peer"
			}
		}
	} else {
		foreach {key peer} $foundpeers {
			#puts "ml_personhead send key $key peer $peer"
			set speer [split $peer {:}]
			#send [lindex $speer 1] [lindex $speer 2] "HEAD 0 PWT $::me"
			ml_genc [lindex $speer 0] [lindex $speer 1] [lindex $speer 2] "HEAD 0 PSH $::me" 0
		}
	}
}

proc ml_genc {peerid host port msg response} {
	puts "ml_genc peerid $peerid host $host port $port"
	set host [string map {{localhost} {127.0.0.1}} $host]
	#puts "ml_genc peerid $peerid host $host port $port msg [string range $msg 0 127]"
	if { $peerid == {} } {	
		#puts "ml_genc empty peerid"
		set peers [lsort -unique [lsearch -all -inline [array get ::peerstore] "*:$host:$port"]]
		#puts "ml_genc peers $peers"
		set len [llength $peers]
		#puts "ml_genc len $len"
		if { $len != 0 } {
			set in [expr "[clock microseconds]%$len"]
			set peerid [lindex [split [lindex $peers $in] {:}] 0]
		}
	} elseif { $host == {} && $port == {} && $response != 1} {
		#puts "ml_genc find host and port by peerid $peerid"
		set peers [lsort -unique [lsearch -all -inline [array get ::peerstore] "$peerid:*:*"]]
		set len [llength $peers]
		if { $len != 0 } {
			set in [expr "[clock microseconds]%$len"]
			set host [lindex [split [lindex $peers $in] {:}] 1]
			set port [lindex [split [lindex $peers $in] {:}] 2]
			#puts "ml_genc by peerid found h $host and p $port"
		}
	}
	#puts "ml_genc peerid $peerid"
	set pubkey {}
	set contacts [lsearch -all -inline [array get ::contacts] "*$peerid*"]
	#puts "ml_genc contacts $contacts"
	if { $contacts == {} } {
		sc_get_contacts $peerid
	}
	set contact {}
	set c {}
	foreach ccontact $contacts {
		set cc [contact_to_dict $ccontact]
		if { $cc != "" } {	
			set pubkey [dict get $cc pubkey]
			if { $pubkey == "?" } {
				sc_get_contacts $peerid
				continue
			}
			set c $cc
			set contact $ccontact
		} else {
			continue
		}
	}
	#puts "ml_genc contact $contact"
	#puts "ml_genc c $c"
	#puts "ml_genc peerid $peerid sum [::sha1::sha1 $pubkey]"
	if { $pubkey != "" && $peerid != "" && [::sha1::sha1 $pubkey] == $peerid } {
		puts "ml_genc encrypting msg"
		set ids [lsort [list $::me $peerid]]
		set hash "g-[lindex $ids 0]-[lindex $ids 1]"
		if { [llength [array names ::keys "myaeskey_$hash"]] > 0 && $response == 1} {
			catch {
			set emsg [enc_msg $pubkey $msg 0 $hash]
			}
		} else {
			catch {
			set emsg [enc_msg $pubkey $msg 1 $hash]
			}
		}
		if { $emsg == "" } {
			return
		}
		if { $response != 1 } { 
			array unset ::tcp_fail "$host,$port"
			send $host $port "GENC 0 [string trim $emsg]"
		} else {
			return "GENC 0 [string trim $emsg]"
		}
	} elseif { $::plain_allowed == 1 } {
		puts "ml_genc sending plain msg"
		if { $response != 1 } {
			array unset ::tcp_fail "$host,$port"
			send $host $port $msg
		} else {
			return $msg
		}
	} else {
		puts "ml_genc can't send contacts"
		#sc_get_contacts [prep_contact_keys $peerid]
		#puts "ml_genc can't send contacts $contacts contact $contact c $c pubkey $pubkey peerid $peerid"
	}
}

proc ml_grouphead {group} {
	set start [clock microseconds]
	set g [ml_groupdict $group]
	set gid [dict get $g gid]
	set sid [wrap "$::menick:$::me"]
	set end [clock microseconds]
	#puts "ml_grouphead timemltostart [expr {$end-$start}]"
	set start [clock microseconds]
	gchat_send $gid $sid [clock microseconds] "RENEW" "ask"
	set end [clock microseconds]
	#puts "ml_grouphead timemlsendrenew [expr {$end-$start}]"
	set gpeerid [dict get $g peerid]
	set start [clock microseconds]
	# ask gid sources for sources
	if { $::group_host_mode != 1 || $gpeerid == $::me } {
		set src [lsort -unique -stride 2 -index end [array get ::sources "$gid*"]]
	} else {
		set src [list $gid $gpeerid]
	}
	set end [clock microseconds]
	#puts "ml_grouphead timemlfindsources [expr {$end-$start}]"
	#puts "ml_grouphead src $src"
	set start [clock microseconds]
	set gpeers {}
	foreach {key val} $src {
		lappend gsrc $val
		foreach {pkey pval} [lsort -unique -stride 2 -index end [array get ::peerstore "$val*"]] {
			set speer [split $pval {:}]
			puts "asking src peer $pkey=$pval for our mail contact"
			#send [lindex $speer 1] [lindex $speer 2] "HEAD 0 SRC $gid"
			after idle [list ml_genc [lindex $speer 0] [lindex $speer 1] [lindex $speer 2] "HEAD 0 SRC $gid" 0]
			after idle [list ml_genc [lindex $speer 0] [lindex $speer 1] [lindex $speer 2] "MAIL 0 SRC $gid 1 $::me" 0]
			lappend gpeers $pval
		}
	}
	set end [clock microseconds]
	#puts "ml_grouphead timemlsrc [expr {$end-$start}]"
	# ask known group sources for sources
	if { $::group_host_mode != 1 || $gpeerid == $::me } {
		lappend gsrc {*}[lrange [ml_get_srcs $gid] 2 end]
	} else {
		lappend gsrc $gpeerid
	}
	set start [clock microseconds]
	#puts "ml_grouphead gsrc $gsrc"
	foreach val $gsrc {
		if { [string length $val] != 40 } {
			continue
		}
		set peers [lsort -unique -stride 2 -index end [array get ::peerstore "$val*"]]
		if { [llength $peers] == 0 } {
			set peers [closest_in_buckets $val 2]
			puts "chat peers $peers"
			foreach peer $peers {
				set speer [split $peer {:}]
				puts "asking $peer for our group source"
				if {[llength $speer] == 3} {
					after idle [list str_start [str_create FIND_NODE [lindex $speer 1] [lindex $speer 2] $val none]]
				}
			}
		}
		foreach {pkey pval} $peers {
			set speer [split $pval {:}]
			puts "asking gsrc peer $pkey=$pval for our mail contact"
			#send [lindex $speer 1] [lindex $speer 2] "HEAD 0 SRC $gid"
			if { $::group_host_mode != 1 || $gpeerid == $::me } {
				after idle [list ml_genc [lindex $speer 0] [lindex $speer 1] [lindex $speer 2] "MAIL 0 SRC $gid 1 $::me" 0]
			}
			after idle [list ml_genc [lindex $speer 0] [lindex $speer 1] [lindex $speer 2] "HEAD 0 SIG $gid" 0]
			if { $::group_host_mode != 1 || $gpeerid == $::me } {
				after idle [list ml_genc [lindex $speer 0] [lindex $speer 1] [lindex $speer 2] "HEAD 0 SRC $gid" 0]
			}
		}
	}
	set end [clock microseconds]
	#puts "ml_grouphead timemlsig [expr {$end-$start}]"
	# ask DHT for sources, but don't wait
	if { $::group_host_mode != 1 || $gpeerid == $::me } {
		after idle [list sc_get_sources $gid]
	}
	set start [clock microseconds]
	# send HEAD to each known group member
	foreach val $gsrc {
		if { [string length $val] != 40 } {
			continue
		}
		foreach {pkey pval} [lsort -unique -stride 2 -index end [array get ::peerstore "$val*"]] {
			set speer [split $pval {:}]
			puts "asking gsrc peer $pkey=$pval for our group"
			#send [lindex $speer 1] [lindex $speer 2] "HEAD 0 DIG $gid"
			after idle [list ml_genc [lindex $speer 0] [lindex $speer 1] [lindex $speer 2] "MAIL 0 SRC $gid 1 $::me" 0]
			after idle [list ml_genc [lindex $speer 0] [lindex $speer 1] [lindex $speer 2] "HEAD 0 DIG $gid" 0]
		}
	}
	set end [clock microseconds]
	#puts "ml_grouphead timemldig [expr {$end-$start}]"
	# vwait for some flag to know that we've received enough answers OR 30 seconds have passed
	# fill msglist for display 
	after 200 [list ml_showlist $gid]
	#after 1000 [list ml_showlist $gid]
	#after 5000 [list ml_showlist $gid]
}

proc ml_showhier {hdrlist} {
	dom createDocument root root

	foreach hdr $hdrlist {
		set hash {}

		catch { set hash [dict get [header_to_dict $hdr] hash] }
		if { $hash == "" } {
			continue
		}

		ml_hierfillrec $root $hash

	}

	ml_disphierrec $root 0

}

proc ml_disphierrec {node dep} {
	foreach child [$node childNodes] {

		set name {}
		set subj {}
		set name [$child nodeName]
		set eml {}
		set hash {}

		if { $name == "name" } {
			set subj [$child nodeValue]
		} elseif { $val == "eml" } {
			set eml [$child nodeValue]
		} else {
			ml_disphierrec $child [expr "$dep+1"]
		}
		if { $subj != {} && $val != {} } {
			puts "DISPHIER $node $subj [string range $eml 0 15]..."
		}
	}
}

proc ml_hierfillrec {root hash} {

	set eml [ml_get_eml $hash]
	set bodylines [split $eml "\n"]
	set sline [lindex $eml 2]
	set pline [lindex $eml 5]
	set s [string range $pline 0 2]
	
	if { $s == "HDR" } {
		set phash [lindex $pline 1]
		ml_hierfillrec $root $phash
	}	else {
		set phash $root
	}
	
	$phash appendChild $hash
	$hash appendChild name
	$hash appendChild eml
	[[$hash firstChild] nodeValue $name]
	[[$hash lastChild] nodeValue $eml]

}

proc ml_showlist {group} {
	set ::ml_topline "group: $::ml_cur_group_l"
	set ::msglist_l {}
	set ::msglist_k {}
	puts "filling msglist"
	if { $::searchfield != "" } {
		set reg [string map {{ } {.*}} $::searchfield]
	} else {
		set reg {.*}
	}
	puts "regex is $reg"
	set sorted {}
	foreach hdr [lrange [ml_get_hdrs $group] 2 end] {
		set h [header_to_dict $hdr]	
		if { $h == "" } {
			continue
		}
		if { [regexp $reg $h] == 0 } {
			continue	
		}
		set t "[clock format [dict get $h epoch] -format {%Y-%m-%d %H:%M:%S}] | [string range [dict get $h from] 0 3] | [string range [dict get $h nickname] 0 11] | [string range [dict get $h hash] 0 3] | [string range [dict get $h subject] 0 31] ([dict get $h len])"
		lappend sorted $t $hdr [dict get $h epoch]
	}
	foreach {title hdr epoch} [lsort -decreasing -stride 3 -index end $sorted] {
		lappend ::msglist_l $title
		lappend ::msglist_k $hdr
	}

	#ml_showhier $::msglist_k
}

proc ml_groupdict {group} {
	if {[regexp -all {:} $group] != 6} {
		return
	}
	set h [split $group {:}]
	set d {}
	dict set d gid [lindex $h 0]
	dict set d name [unwrap [lindex $h 1]]
	dict set d desc [unwrap [lindex $h 2]] 
	dict set d pkey [unwrap [lindex $h 3]]
	dict set d epoch [lindex $h 4]
	dict set d peerid [lindex $h 5]
	dict set d psig [unwrap [lindex $h 6]]
	return $d
}

proc ml_dictgroup {d} {
	set h "[dict get $d gid]:[wrap [dict get $d name]]:[wrap [dict get $d desc]]:[wrap [dict get $d pkey]]:[dict get $d epoch]:[dict get $d peerid]:[wrap [dict get $d psig]]"
	return $h
}

proc wrap {str} {
	set ret {?}
	catch {
		set ret [::base64::encode -maxlen 0 [encoding convertto utf-8 $str]]
	} res
	#puts "catch wrap $res"
	return $ret
}

proc unwrap {str} {
	set ret {?}
	catch {
		set ret [encoding convertfrom utf-8 [::base64::decode $str]] 
	} res
	#puts "catch unwrap $res"
	#if { $ret == {} } {
	#	puts "unwrap failure"
	#}
	return $ret
}

proc ml_get_srcs {grp} {
	set path [file join $::filepath "mlsrc" $grp]
	if { [file exists $path] == 0 } {
		puts "ml_get_srcs path $path doesn't exist"
		return "$grp 0 "
	} 
	set f [open $path r]
	set cnt 0
	set srcs {}
	while { [gets $f line] >= 0 } {
		lappend srcs $line
	}
	foreach {key val} [array get ::sources "$grp*"] {
		set p [split $val {:}]
		if { [llength $p] == 1 } {
			lappend srcs [lindex $p 0]
		}
	}
	#set srcs [lsearch -all -inline -not [lsort -unique $srcs] $grp]
	set srcs [lsort -unique $srcs]
	set cnt [llength $srcs]
	close $f
	return "$grp $cnt $srcs"
} 

proc ml_get_personhdrs {peerid} {
	set path [file join $::filepath "mlphdr" $peerid]
	if { [file exists $path] == 0 } {
		return
	} 
	set f [open $path r]
	set cnt 0
	set hdrs {}
	while { [gets $f line] >= 0 } {
		lappend hdrs $line
	}
	set hdrs [lsort -unique $hdrs]
	set hdrs [ml_filter_del $hdrs]
	set cnt [llength $hdrs]
	close $f
	return "$::me $cnt $hdrs"
} 

proc ml_get_hdrs {grp} {
	set path [file join $::filepath "mlhdr" $grp]
	if { [file exists $path] == 0 } {
		puts "ml_get_hdrs path $path doesn't exist"
		return
	} 
	set f [open $path r]
	set cnt 0
	set hdrs {}
	while { [gets $f line] >= 0 } {
		lappend hdrs $line
	}
	set hdrs [lsort -unique $hdrs]
	set hdrs [ml_filter_del $hdrs]
	set cnt [llength $hdrs]
	close $f
	return "$grp $cnt $hdrs"
} 

proc ml_add_del {hash} {
	set hash [lindex [split $hash {:}] 0]
	set path [file join $::filepath mldel]
	set f [open $path a]
	puts $f $hash
	flush $f
	close $f
	ml_del_eml $hash
}

proc ml_filter_del {hashes} {
	#puts "ml_filter_del args $hashes"
	set path [file join $::filepath mldel]
	if { ![file exists $path] } {
		#puts "ml_filter_del no del file"
		return $hashes
	}
	set f [open $path r]
	set del [split [read $f] "\n"]
	close $f
	set ret {}
	foreach hash $hashes {
		#puts "ml_filter_del deleted $del tofind [lindex [split $hash {:}] 0]"
		set res [lsearch $del [lindex [split $hash {:}] 0]]
		#puts "ml_filter_del res $res"
		if { $res == {} || $res == -1 } {
			lappend ret $hash
		}
	}
	#puts "filtered hashes $ret"
	return $ret
}

proc ml_del_eml {hash} {
	puts "deleting letter $hash"
	set prefix [lindex [split $hash {_}] 0]
	set end [lindex [split $hash {_}] 1]
	if { $end != $prefix } {
		set path [file join $::filepath mailnews $prefix $end]
	} else {
		set path [file join $::filepath mailnews $hash]
	}
	if { [file exists $path] == 0 } {
		puts "no letter $path stored"
		return -1
	}
	catch {
	file delete $path
	}
}

proc ml_get_eml {hash} {
	puts "getting letter $hash"
	if { [ml_filter_del $hash] == {} } {
		puts "letter $hash in delete list, not stored"
		return -1
	}
	set prefix [lindex [split $hash {_}] 0]
	set end [lindex [split $hash {_}] 1]
	if { $end != $prefix } {
		set path [file join $::filepath mailnews $prefix $end]
	} else {
		set path [file join $::filepath mailnews $hash]
	}
	if { [file exists $path] == 0 } {
		puts "no letter $path stored"
		return -1
	}
	set f [open $path r]
	fconfigure $f -translation binary
	set ret [read $f]
	set chash [::sha1::sha1 $ret]
	close $f
	if { $end == $prefix && $hash != $chash } {	
		puts "wrong hash $hash != $chash"
		return -1
	}
	return $ret
}

proc ml_add_srcs {grp srcs} {
	puts "ml_add_srcs grp $grp srcs $srcs"
	set t_srcs {}
	set t_srcs [lrange [ml_get_srcs $grp] 2 end]
	lappend t_srcs {*}$srcs
	foreach {key val} [array get ::sources "$grp*"] {
		set p [split $val {:}]
		if { [llength $p] == 1 } {
			lappend t_srcs [lindex $p	0]
		}
	}
	set t_srcs [lsearch -all -inline -not [lsort -unique $t_srcs] $grp]
	puts "ml_add_srcs t_srcs $t_srcs"
	set path [file join $::filepath "mlsrc" $grp]
	set f [open $path w]
	foreach src $t_srcs {
		puts $f $src
	}
	flush $f
	close $f
}

proc ml_add_personhdrs {peerid hdrs} {
	set new_hdrs [ml_get_personhdrs $peerid]
	lappend new_hdrs {*}$hdrs
	set path [file join $::filepath "mlphdr" $peerid]
	set f [open $path w]
	foreach hdr $new_hdrs {
		puts $f $hdr
	}
	flush $f
	close $f
}

proc ml_add_hdrs {grp hdrs} {
	set new_hdrs [ml_get_hdrs $grp]
	lappend new_hdrs {*}$hdrs
	set path [file join $::filepath "mlhdr" $grp]
	set f [open $path w]
	foreach hdr $new_hdrs {
		if { [header_to_dict $hdr] != "" } {  
			puts $f $hdr
		}
	}
	flush $f
	close $f
}

proc ml_add_eml {eml} {
	set hash [::sha1::sha1 $eml]
	if { [ml_filter_del $hash] == {} } {
		puts "letter $hash in delete list, not stored"
		return -1
	}
	set path [file join $::filepath "mailnews" $hash]
	if { [file exists $path] == 1 } {
		puts "letter $hash already stored"
		return -1
	}
	set f [open $path w]
	fconfigure $f -translation binary
	puts -nonewline $f $eml
	flush $f
	close $f
}

proc dl_reqdict {req} {
	if {[regexp -all {:} $req] != 3} {
		return
	}
	set r [split $req {:}]
	set d {}
	dict set d filename [unwrap [lindex $r 0]]
	dict set d filehash [lindex $r 1]	
	dict set d offset [lindex $r 2]	
	dict set d len [lindex $r 3]	
	#puts "dl_reqdict $r TO $d"
	return $d
}

proc dl_dictreq {d} {
	#set r [::base64::encode -maxlen 0 [encoding convertto utf-8 [dict get $d filename]]]:[dict get $d filehash]:[dict get $d offset]:[dict get $d len] 
	set r [wrap [dict get $d filename]]:[dict get $d filehash]:[dict get $d offset]:[dict get $d len] 
	#puts "dl_dictreq $d TO $r"
	return $r
}

proc dl_read {req} {
	#puts "dl_read"
	set r [dl_reqdict $req]	
	#puts "dl_read r $r"
	set filehash [dict get $r filehash]
	set offset [dict get $r offset]
	set len [dict get $r len]
	set detail [lindex [array get ::file_by_hash $filehash] 1]
	#puts "detail($filehash): $detail"
	set local_filename [unwrap [dict get $detail name]]
	#puts "dl_read local_filename $local_filename"
	if { [string length $detail] == 0 } {
		#puts "dl_read no such indexed file"
		return
	}
	if { [file exists $local_filename] != 1 || [file isfile $local_filename] != 1} {
		#puts "dl_read no such file by indexed name"
		return
	}
	set f [open $local_filename r]
	if { $f == "" } {
		puts "dl_read file not opened"
		return
	}
	fconfigure $f -translation binary
	#puts "dl_read seek $f $offset start"
	seek $f $offset start
	set data [read $f $len]
	close $f
	return $req,[wrap $data]
}

proc dl_write {body} {
	#puts "dl_write"
	set req [lindex [split $body {,}] 0]
	set data [unwrap [lrange [split $body {,}] 1 end]]
	set r [dl_reqdict $req]	
	#puts "dl_write r $r"
	set filehash [dict get $r filehash]
	set filename [lindex [file split [dict get $r filename]] end]
	set offset [dict get $r offset]
	set len [dict get $r len]
	set local_filename [file join $::filepath "temp" $filename]
	if { [file exists $local_filename] == 1 && [file isfile $local_filename] != 1} {
		puts "not a file"
		return
	}
	if { $::dlstate_by_hash($filehash,state) != "RUN" } {
		puts "not running"
		return
	}	
	set f [open $local_filename a+]
	fconfigure $f -translation binary
	#puts "dl_write seek $f $offset start"
	seek $f $offset start 
	puts -nonewline $f $data
	flush $f
	close $f
	#puts "dl_write "
	set top [lindex [array get ::dlstate_by_hash "$filehash,top"] 1]
	array set ::dlstate_by_hash [list $filehash,top [expr {$offset+$len}]]
	array set ::dlstate_by_hash [list $filehash,last [clock seconds]]
	#puts "dl_write dl_run"
	after 200 [list dl_run $filehash]
	return 0
}

proc dl_getpeers {hash cnt} {
	puts "dl_getpeers"
	set have [array get ::sources "$hash*"]
	lappend have {*}[array get ::sources "[::sha1::sha1 $hash]*"]
	set cnt [expr "$cnt+1"]
	if { [llength $have] == 0 && $cnt < 20 } {
		puts "dl_getpeers sc_get_sources $hash"
		sc_get_sources $hash
		sc_get_sources [::sha1::sha1 $hash]
		after 5000 [list dl_getpeers $hash $cnt]
	} else {
		puts "dl_getpeers have $have"
		set peers {}
		foreach {key value} $have {
			puts "dl_getpeers have is $key $value"
			foreach {pkey pvalue} [lsort -unique -stride 2 -index end [array get ::peerstore "$value*" ]] {
				puts "dl_getpeers peer $pvalue"
				lappend peers $pvalue
			}
		}
		if { [llength $peers] > 0 } {
			puts "peers $peers"
			array set ::dlstate_by_hash [list $hash,plainpeers $peers]
			after 100 [list dl_run $hash]
		}
	}
}

proc dl_add {req buddyhash plainpeers} {
	puts "dl_add"
	set r [dl_reqdict $req]
	set hash [dict get $r filehash]
	dict set r filename [lindex [file split [dict get $r filename]] end]
	array set ::dl_by_hash [list $hash $r]
	array set ::dlstate_by_hash [list $hash,state "STOP"]
	# if it's a buddy transfer, we set a buddy to give us data 
	array set ::dlstate_by_hash [list $hash,buddy $buddyhash]
	# and if we are given a list of peers, then we add them
	if { [llength $plainpeers] == 0 && $buddyhash == "" } {
		puts "dl_getpeers $hash 0"
		dl_getpeers $hash 0
	}
	array set ::dlstate_by_hash [list $hash,plainpeers $plainpeers]
	array set ::dlstate_by_hash [list $hash,top 0]
	array set ::dlstate_by_hash [list $hash,last [clock seconds]]
	return $hash
}

proc dl_start {hash} {
	if { $hash == "" } {
		return
	}
	puts "dl_start"
	array set ::dlstate_by_hash [list $hash,state "RUN"]
	array set ::dlstate_by_hash [list $hash,last [clock seconds]]
	dl_getpeers $hash 0
	dl_check $hash
	after 50 [list dl_run $hash]
}

proc dl_check {hash} {
	if { $hash == "" } {
		return
	}
	if { [llength [array names ::dlstate_by_hash "$hash*"]] == 0 } {
		puts "no such dlstate"
		return
	}
	if { $::dlstate_by_hash($hash,state) != "RUN" } {
		puts "not running"
		return
	}	
	if { [array get ::dlstate_by_hash $hash,buddy] == "" && [llength [array get ::dlstate_by_hash $hash,plainpeers ]] == 0 } {
		puts "no buddy" 
		#return
	}	
	if { $::dlstate_by_hash($hash,last) < [expr "[clock seconds] - 10"] } {
		dl_run $hash
	}
	after 1000 [list dl_check $hash]
}

proc dl_run {hash} {
	puts "dl_run"
	if { $::dlstate_by_hash($hash,state) != "RUN" } {
		puts "not running"
		return
	}	
	if { [array get ::dlstate_by_hash $hash,buddy] == "" && [llength [array get ::dlstate_by_hash $hash,plainpeers ]] == 0 } {
		puts "no buddy" 
		#return
	}	
	set r [lindex [array get ::dl_by_hash $hash] 1]
	set size [dict get $r len]
	if { $::dlstate_by_hash($hash,top) < $size } {
		set cr {}
		dict set cr filehash $hash
		dict set cr filename [lindex [file split [dict get $r filename]] end]
		dict set cr offset $::dlstate_by_hash($hash,top)
		# 32mb+ -> chunk 16mb
		if { $size >= 33554432 } {
			set chunksize 16777216 
		} else {
			set chunksize $::default_chunksize
		}
		dict set cr len $chunksize
		set req [dl_dictreq $cr]
		# normal transfer between buddies
		if { [lindex [array get ::dlstate_by_hash $hash,buddy] end] != "" } {
			puts "dl_run normal transfer to  $::dlstate_by_hash($hash,buddy)"
			chat_send $::dlstate_by_hash($hash,buddy) {} "GET" $req
		# ask peers - choose random one
		} elseif { [llength [array get ::dlstate_by_hash $hash,plainpeers ]] != 0 } {
			set peernum [llength $::dlstate_by_hash($hash,plainpeers)]
			puts "dl_run plain transfer peers $peernum"
			if { $peernum != 0 } {
				set plainpeer_index [expr "[clock microseconds]%$peernum"]
			} else {
				set plainpeer_index 0
			}
			puts "dl_run plain transfer index $plainpeer_index"
			set plainpeer [lindex $::dlstate_by_hash($hash,plainpeers) $plainpeer_index]
			puts "dl_run plain transfer $plainpeer"
			set speer [split $plainpeer {:}]
			#send [lindex $speer 1] [lindex $speer 2] "GET 0 $req"
			ml_genc [lindex $speer 0] [lindex $speer 1] [lindex $speer 2] "GET 0 $req" 0
		}
	} elseif {$::dlstate_by_hash($hash,top) >= $size} {
		array unset ::dlstate_by_hash "$hash,id"
		dl_stop $hash
		after 5000 [list dl_finish $hash]
	} else {
		puts "dl_run weird top $hash , top $::dlstate_by_hash($hash,top), size $size"
	}
}

proc dl_stop {hash} {
	if { $hash == "" } {
		return
	}
	puts "dl_stop"
	array set ::dlstate_by_hash [list $hash,state "STOP"]
}

proc dl_finish {hash} {
	if { $hash == "" } {
		return
	}
	puts "dl_finish"
	set r [lindex [array get ::dl_by_hash $hash] end]
	if { $r == "" } {
		puts "dl_finish no dl"
		return
	}
	set filename [lindex [file split [dict get $r filename]] end]
	set local_filename [file join $::filepath "temp" $filename]
	puts "dl_finish filename $filename"
	puts "dl_finish local_filename $local_filename"
	if { [file exists $local_filename] == 0 } {
		puts "dl_finish $local_filename doesn't exist"
		return	
	}
	set final_filename [file join $::filepath "downloads" $filename]
	puts "dl_finish final_filename $final_filename"
	puts "dl_finish hashing file $local_filename"
	set lhash [::sha1::sha1 -hex -file $local_filename]
	if { $lhash != $hash } {
		puts "error checking downloaded file"
		array set ::dlstate_by_hash [list $hash,state "ERROR"]
		return
	}
	file rename $local_filename $final_filename
	array set ::dlstate_by_hash [list $hash,state "DONE"]
	after 18000 [list dl_del $hash]
}

proc dl_del {hash} {
	if { $hash == "" } {
		return	
	}
	puts "dl_del"
	set r [lindex [array get ::dl_by_hash $hash] 1]
	if { $r == "" } {
		return
	}
	if { [lindex [array get ::dlstate_by_hash $hash,state] 1] != "DONE" } {
		set filename [dict get $r filename]
		set local_filename [file join $::filepath "temp" $filename]
		if { [file exists $local_filename] == 1 } {
			file delete $local_filename
		}
	}
	array unset ::dl_by_hash $hash
	array unset ::dlstate_by_hash $hash,*
}

### end
main $argv
