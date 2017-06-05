# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

install: install-fonts
.PHONY: install-fonts

install-fonts:
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/DejaVu-2.34
	install -p -m 644 Fonts/DejaVu-2.34/*.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/DejaVu-2.34
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/Asana-Math
	install -p -m 644 Fonts/Asana-Math/Asana-Math.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Asana-Math/Asana-Math.ttf
	install -p -m 644 Fonts/Asana-Math/Asana-Math.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Asana-Math/Asana-Math.otf
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/Philosopher
	install -p -m 644 Fonts/Philosopher/Philosopher-BoldItalic.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Philosopher/Philosopher-BoldItalic.otf
	install -p -m 644 Fonts/Philosopher/Philosopher-Regular.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Philosopher/Philosopher-Regular.otf
	install -p -m 644 Fonts/Philosopher/Philosopher-Italic.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Philosopher/Philosopher-Italic.otf
	install -p -m 644 Fonts/Philosopher/Philosopher-Bold.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Philosopher/Philosopher-Bold.otf
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/TexGyreCursor
	install -p -m 644 Fonts/TexGyreCursor/texgyrecursor-bold.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/TexGyreCursor/texgyrecursor-bold.otf
	install -p -m 644 Fonts/TexGyreCursor/texgyrecursor-italic.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/TexGyreCursor/texgyrecursor-italic.otf
	install -p -m 644 Fonts/TexGyreCursor/texgyrecursor-bolditalic.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/TexGyreCursor/texgyrecursor-bolditalic.otf
	install -p -m 644 Fonts/TexGyreCursor/texgyrecursor-regular.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/TexGyreCursor/texgyrecursor-regular.otf
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya
	install -p -m 644 Fonts/Alegreya/AlegreyaSC-Regular.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/AlegreyaSC-Regular.otf
	install -p -m 644 Fonts/Alegreya/AlegreyaSC-Italic.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/AlegreyaSC-Italic.otf
	install -p -m 644 Fonts/Alegreya/Alegreya-BlackItalic.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/Alegreya-BlackItalic.otf
	install -p -m 644 Fonts/Alegreya/Alegreya-Bold.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/Alegreya-Bold.otf
	install -p -m 644 Fonts/Alegreya/AlegreyaSC-Bold.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/AlegreyaSC-Bold.otf
	install -p -m 644 Fonts/Alegreya/Alegreya-Italic.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/Alegreya-Italic.otf
	install -p -m 644 Fonts/Alegreya/AlegreyaSC-Black.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/AlegreyaSC-Black.otf
	install -p -m 644 Fonts/Alegreya/Alegreya-BoldItalic.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/Alegreya-BoldItalic.otf
	install -p -m 644 Fonts/Alegreya/AlegreyaSC-BoldItalic.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/AlegreyaSC-BoldItalic.otf
	install -p -m 644 Fonts/Alegreya/AlegreyaSC-BlackItalic.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/AlegreyaSC-BlackItalic.otf
	install -p -m 644 Fonts/Alegreya/Alegreya-Black.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/Alegreya-Black.otf
	install -p -m 644 Fonts/Alegreya/Alegreya-Regular.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/Alegreya-Regular.otf
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/BitstreamVeraSansMono
	install -p -m 644 Fonts/BitstreamVeraSansMono/BitstreamVeraSansMono-Oblique.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/BitstreamVeraSansMono/BitstreamVeraSansMono-Oblique.otf
	install -p -m 644 Fonts/BitstreamVeraSansMono/BitstreamVeraSansMono-Roman.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/BitstreamVeraSansMono/BitstreamVeraSansMono-Roman.otf
	install -p -m 644 Fonts/BitstreamVeraSansMono/BitstreamVeraSansMono-BoldOb.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/BitstreamVeraSansMono/BitstreamVeraSansMono-BoldOb.otf
	install -p -m 644 Fonts/BitstreamVeraSansMono/BitstreamVeraSansMono-Bold.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/BitstreamVeraSansMono/BitstreamVeraSansMono-Bold.otf
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/Euler
	install -p -m 644 Fonts/Euler/euler.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Euler/euler.otf
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont
	install -p -m 644 Fonts/FreeFont/FreeSerif.ttf  $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont/FreeSerif.ttf
	install -p -m 644 Fonts/FreeFont/FreeSerifItalic.ttf  $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont/FreeSerifItalic.ttf
	install -p -m 644 Fonts/FreeFont/FreeSerifBold.ttf  $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont/FreeSerifBold.ttf
	install -p -m 644 Fonts/FreeFont/FreeSerifBoldItalic.ttf  $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont/FreeSerifBoldItalic.ttf
	install -p -m 644 Fonts/FreeFont/FreeSans.ttf  $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont/FreeSans.ttf
	install -p -m 644 Fonts/FreeFont/FreeSansOblique.ttf  $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont/FreeSansOblique.ttf
	install -p -m 644 Fonts/FreeFont/FreeSansBold.ttf  $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont/FreeSansBold.ttf
	install -p -m 644 Fonts/FreeFont/FreeSansBoldOblique.ttf  $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont/FreeSansBoldOblique.ttf
	install -p -m 644 Fonts/FreeFont/FreeMono.ttf  $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont/FreeMono.ttf
	install -p -m 644 Fonts/FreeFont/FreeMonoOblique.ttf  $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont/FreeMonoOblique.ttf
	install -p -m 644 Fonts/FreeFont/FreeMonoBold.ttf  $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont/FreeMonoBold.ttf
	install -p -m 644 Fonts/FreeFont/FreeMonoBoldOblique.ttf  $(DESTDIR)/$(INSTALL_FONT_DIR)/FreeFont/FreeMonoBoldOblique.ttf
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/KacstOne
	install -p -m 644 Fonts/KacstOne/KacstOne-Bold.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/KacstOne/KacstOne-Bold.ttf
	install -p -m 644 Fonts/KacstOne/KacstOne.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/KacstOne/KacstOne.ttf
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/AMS
	install -p -m 644 Fonts/AMS/ams.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/AMS/ams.otf

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))

