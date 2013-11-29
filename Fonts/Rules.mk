# Standard things which help keeping track of the current directory
# while include all Rules.mk.
d := $(if $(d),$(d)/,)$(mod)

install: install-fonts
.PHONY: install-fonts

install-fonts:
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/DejaVuSans
	install -p -m 644 Fonts/DejaVuSans/DejaVuSansMono.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/DejaVuSans/DejaVuSansMono.otf
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
	install -p -m 644 Fonts/Alegreya/AlegreyaSC-Regular.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/AlegreyaSC-Regular.ttf
	install -p -m 644 Fonts/Alegreya/AlegreyaSC-Italic.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/AlegreyaSC-Italic.ttf
	install -p -m 644 Fonts/Alegreya/Alegreya-BlackItalic.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/Alegreya-BlackItalic.ttf
	install -p -m 644 Fonts/Alegreya/Alegreya-Bold.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/Alegreya-Bold.ttf
	install -p -m 644 Fonts/Alegreya/AlegreyaSC-Bold.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/AlegreyaSC-Bold.ttf
	install -p -m 644 Fonts/Alegreya/Alegreya-Italic.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/Alegreya-Italic.ttf
	install -p -m 644 Fonts/Alegreya/AlegreyaSC-Black.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/AlegreyaSC-Black.ttf
	install -p -m 644 Fonts/Alegreya/Alegreya-BoldItalic.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/Alegreya-BoldItalic.ttf
	install -p -m 644 Fonts/Alegreya/AlegreyaSC-BoldItalic.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/AlegreyaSC-BoldItalic.ttf
	install -p -m 644 Fonts/Alegreya/AlegreyaSC-BlackItalic.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/AlegreyaSC-BlackItalic.ttf
	install -p -m 644 Fonts/Alegreya/Alegreya-Black.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/Alegreya-Black.ttf
	install -p -m 644 Fonts/Alegreya/Alegreya-Regular.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/Alegreya/Alegreya-Regular.ttf
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/BitstreamVeraSansMono
	install -p -m 644 Fonts/BitstreamVeraSansMono/BitstreamVeraSansMono-Oblique.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/BitstreamVeraSansMono/BitstreamVeraSansMono-Oblique.otf
	install -p -m 644 Fonts/BitstreamVeraSansMono/BitstreamVeraSansMono-Roman.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/BitstreamVeraSansMono/BitstreamVeraSansMono-Roman.otf
	install -p -m 644 Fonts/BitstreamVeraSansMono/BitstreamVeraSansMono-BoldOb.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/BitstreamVeraSansMono/BitstreamVeraSansMono-BoldOb.otf
	install -p -m 644 Fonts/BitstreamVeraSansMono/BitstreamVeraSansMono-Bold.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/BitstreamVeraSansMono/BitstreamVeraSansMono-Bold.otf
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/Euler
	install -p -m 644 Fonts/Euler/euler.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/Euler/euler.otf
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/KacstOne
	install -p -m 644 Fonts/KacstOne/KacstOne-Bold.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/KacstOne/KacstOne-Bold.ttf
	install -p -m 644 Fonts/KacstOne/KacstOne.ttf $(DESTDIR)/$(INSTALL_FONT_DIR)/KacstOne/KacstOne.ttf
	install -p -m 755 -d $(DESTDIR)/$(INSTALL_FONT_DIR)/AMS
	install -p -m 644 Fonts/AMS/ams.otf $(DESTDIR)/$(INSTALL_FONT_DIR)/AMS/ams.otf

# Rolling back changes made at the top
d := $(patsubst %/,%,$(dir $(d)))

