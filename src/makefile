.SUFFIXES: .erl .beam .yrl

 .erl.beam:
	erlc -W $<

 .yrl.erl:
	erlc -W $<

 ERL = erl -boot start_clean

 MODS = ezk_connection ezk_log ezk_message_2_packet ezk_packet_2_message testit

 all: compile

 compile: ${MODS:%=%.beam} moveit

 moveit:
	mv ./*.beam ../ebin/

 clean:
	rm -rf *.beam erl_crash.dump

 subdirs:
	cd src; make
