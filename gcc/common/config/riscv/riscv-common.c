/* Common hooks for RISC-V.
   Copyright (C) 2016-2019 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <sstream>
/* TODO: remove this with the pulp chip struct hack */
#define _WITH_PULP_CHIP_INFO_FUNCT_

#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"
#include "diagnostic-core.h"
#include "config/riscv/riscv-protos.h"

#define RISCV_DONT_CARE_VERSION -1


/* Subset info.  */
struct riscv_subset_t
{
  riscv_subset_t ();

  std::string name;
  int major_version;
  int minor_version;
  struct riscv_subset_t *next;
};

/* Subset list.  */
class riscv_subset_list
{
private:
  /* Original arch string.  */
  const char *m_arch;

  /* Location of arch string, used for report error.  */
  location_t m_loc;

  /* Head of subset info list.  */
  riscv_subset_t *m_head;

  /* Tail of subset info list.  */
  riscv_subset_t *m_tail;

  /* X-len of m_arch. */
  unsigned m_xlen;

  riscv_subset_list (const char *, location_t);

  const char *parsing_subset_version (const char *, unsigned *, unsigned *,
				      unsigned, unsigned, bool);

  const char *parse_std_ext (const char *);

  const char *parse_sv_or_non_std_ext (const char *, const char *,
				       const char *);

public:
  ~riscv_subset_list ();

  void add (const char *, int, int);

  riscv_subset_t *lookup (const char *,
			  int major_version = RISCV_DONT_CARE_VERSION,
			  int minor_version = RISCV_DONT_CARE_VERSION) const;

  std::string to_string () const;

  unsigned xlen() const {return m_xlen;};

  static riscv_subset_list *parse (const char *, location_t);

};

static const char *riscv_supported_std_ext (void);

static riscv_subset_list *current_subset_list = NULL;

riscv_subset_t::riscv_subset_t ()
  : name (), major_version (0), minor_version (0), next (NULL)
{
}

riscv_subset_list::riscv_subset_list (const char *arch, location_t loc)
  : m_arch (arch), m_loc (loc), m_head (NULL), m_tail (NULL), m_xlen (0)
{
}

riscv_subset_list::~riscv_subset_list ()
{
  if (!m_head)
    return;

  riscv_subset_t *item = this->m_head;
  while (item != NULL)
    {
      riscv_subset_t *next = item->next;
      delete item;
      item = next;
    }
}

/* Add new subset to list.  */

void
riscv_subset_list::add (const char *subset, int major_version,
			int minor_version)
{
  riscv_subset_t *s = new riscv_subset_t ();

  if (m_head == NULL)
    m_head = s;

  s->name = subset;
  s->major_version = major_version;
  s->minor_version = minor_version;
  s->next = NULL;

  if (m_tail != NULL)
    m_tail->next = s;

  m_tail = s;
}

/* Convert subset info to string with explicit version info.  */

std::string
riscv_subset_list::to_string () const
{
  std::ostringstream oss;
  oss << "rv" << m_xlen;

  bool first = true;
  riscv_subset_t *subset = m_head;

  while (subset != NULL)
    {
      if (!first)
	oss << '_';
      first = false;

      oss << subset->name
	  << subset->major_version
	  << 'p'
	  << subset->minor_version;
      subset = subset->next;
    }

  return oss.str ();
}

/* Find subset in list with version checking, return NULL if not found.
   major/minor version checking can be ignored if major_version/minor_version
   is RISCV_DONT_CARE_VERSION.  */

riscv_subset_t *
riscv_subset_list::lookup (const char *subset, int major_version,
			   int minor_version) const
{
  riscv_subset_t *s;

  for (s = m_head; s != NULL; s = s->next)
    if (strcasecmp (s->name.c_str (), subset) == 0)
      {
	if ((major_version != RISCV_DONT_CARE_VERSION)
	    && (s->major_version != major_version))
	  return NULL;

	if ((minor_version != RISCV_DONT_CARE_VERSION)
	    && (s->minor_version != minor_version))
	  return NULL;

	return s;
      }

  return s;
}

/* Return string which contains all supported standard extensions in
   canonical order.  */

static const char *
riscv_supported_std_ext (void)
{
  return "mafdqlcbjtpvn";
}

/* Parsing subset version.

   Return Value:
     Points to the end of version

   Arguments:
     `p`: Current parsing position.
     `major_version`: Parsing result of major version, using
      default_major_version if version is not present in arch string.
     `minor_version`: Parsing result of minor version, set to 0 if version is
     not present in arch string, but set to `default_minor_version` if
     `major_version` using default_major_version.
     `default_major_version`: Default major version.
     `default_minor_version`: Default minor version.
     `std_ext_p`: True if parsing std extension.  */

const char *
riscv_subset_list::parsing_subset_version (const char *p,
					   unsigned *major_version,
					   unsigned *minor_version,
					   unsigned default_major_version,
					   unsigned default_minor_version,
					   bool std_ext_p)
{
  bool major_p = true;
  bool assigned = false;
  unsigned version = 0;
  unsigned major = 0;
  unsigned minor = 0;
  char np;

  for (; *p; ++p)
    {
      if (*p == 'p')
	{
	  np = *(p + 1);

	  if (!ISDIGIT (np))
	    {
	      /* Might be beginning of `p` extension.  */
	      if (std_ext_p)
		{
		  *major_version = version;
		  *minor_version = 0;
		  return p;
		}
	      else
		{
		  error_at (m_loc, "%<-march=%s%>: Expect number "
			    "after %<%dp%>.", m_arch, version);
		  return NULL;
		}
	    }

	  major = version;
	  major_p = false;
	  version = 0;
	}
      else if (ISDIGIT (*p))
	{
	  version = (version * 10) + (*p - '0');
	  assigned = true;
	}
      else
	break;
    }

  if (major_p)
    major = version;
  else
    minor = version;

  if ((major == 0 && minor == 0) && !assigned)
    {
      /* We didn't find any version string, use default version.  */
      *major_version = default_major_version;
      *minor_version = default_minor_version;
    }
  else
    {
      *major_version = major;
      *minor_version = minor;
    }
  return p;
}

/* Parsing function for standard extensions.

   Return Value:
     Points to the end of extensions.

   Arguments:
     `p`: Current parsing position.  */

const char *
riscv_subset_list::parse_std_ext (const char *p)
{
  const char *all_std_exts = riscv_supported_std_ext ();
  const char *std_exts = all_std_exts;

  unsigned major_version = 0;
  unsigned minor_version = 0;
  char std_ext = '\0';

  /* First letter must start with i, e or g.  */
  switch (*p)
    {
    case 'i':
      p++;
      p = parsing_subset_version (p, &major_version, &minor_version,
				  /* default_major_version= */ 2,
				  /* default_minor_version= */ 0,
				  /* std_ext_p= */ true);
      add ("i", major_version, minor_version);
      break;

    case 'e':
      p++;
      p = parsing_subset_version (p, &major_version, &minor_version,
				  /* default_major_version= */ 1,
				  /* default_minor_version= */ 9,
				  /* std_ext_p= */ true);

      add ("e", major_version, minor_version);

      if (m_xlen > 32)
	{
	  error_at (m_loc, "%<-march=%s%>: rv%de is not a valid base ISA",
		    m_arch, m_xlen);
	  return NULL;
	}
      break;

    case 'g':
      p++;
      p = parsing_subset_version (p, &major_version, &minor_version,
				  /* default_major_version= */ 2,
				  /* default_minor_version= */ 0,
				  /* std_ext_p= */ true);
      add ("i", major_version, minor_version);

      for (; *std_exts != 'q'; std_exts++)
	{
	  const char subset[] = {*std_exts, '\0'};
	  add (subset, major_version, minor_version);
	}
      break;

    default:
      error_at (m_loc, "%<-march=%s%>: first ISA subset must be %<e%>, "
		"%<i%> or %<g%>", m_arch);
      return NULL;
    }

  while (*p)
    {
      char subset[2] = {0, 0};

      if (*p == 'x' || *p == 's')
	break;

      if (*p == '_')
	{
	  p++;
	  continue;
	}

      std_ext = *p;

      /* Checking canonical order.  */
      while (*std_exts && std_ext != *std_exts)
	std_exts++;

      if (std_ext != *std_exts)
	{
	  if (strchr (all_std_exts, std_ext) == NULL)
	    error_at (m_loc, "%<-march=%s%>: unsupported ISA subset %<%c%>",
		      m_arch, *p);
	  else
	    error_at (m_loc,
		      "%<-march=%s%>: ISA string is not in canonical order. "
		      "%<%c%>", m_arch, *p);
	  return NULL;
	}

      std_exts++;

      p++;
      p = parsing_subset_version (p, &major_version, &minor_version,
				  /* default_major_version= */ 2,
				  /* default_minor_version= */ 0,
				  /* std_ext_p= */ true);

      subset[0] = std_ext;

      add (subset, major_version, minor_version);
    }
  return p;
}

/* Return string which contains all supported non-standard extensions in
   canonical order.  */

bool
riscv_is_supported_pulp_ext (const char *ext)
{
  const char *pulp_exts[] = {
    /* pulp extenion groupings */
    "xpulpslim"
    "xriscv",
    "xpulpv",
    "xgap",
    /* pulp extension subsets */
    "xpulphwloop",
    "xpulppostmod",
    "xpulpindregreg", /* does not affect assembly only code gen */
    "xpulpmac",
    "xpulppartmac",
    "xpulpmacalt",
    "xpulpmacsi",
    "xpulpmacrnhi",
    "xpulpmulrnhi",
    "xpulpminmax",
    "xpulpabs",
    "xpulpbitop",
    "xpulpbitopsmall",
    "xpulpslet",
    "xpulpvectall",
    "xpulpvect",
    "xpulpvectshufflepack", /* does not affect assembly only code gen*/
    "xpulpvectgap",
    "xpulpbr",
    "xpulpclip",
    "xpulpaddsubrn",
    "xpulpelw",
    NULL
  };

  for (const char **pulp_ext = pulp_exts; *pulp_ext != 0; pulp_ext++)
    if (!strcmp (*pulp_ext, ext))
      return true;

  return false;
}

/* Parsing function for non-standard and supervisor extensions.

   Return Value:
     Points to the end of extensions.

   Arguments:
     `p`: Current parsing position.
     `ext_type`: What kind of extensions, 'x', 's' or 'sx'.
     `ext_type_str`: Full name for kind of extension.  */

const char *
riscv_subset_list::parse_sv_or_non_std_ext (const char *p,
					    const char *ext_type,
					    const char *ext_type_str)
{
  unsigned major_version = 0;
  unsigned minor_version = 0;
  size_t ext_type_len = strlen (ext_type);
  // printf("balasr: trying to parse non standard\n");

  while (*p)
    {
      if (*p == '_')
	{
	  p++;
	  continue;
	}

      if (strncmp (p, ext_type, ext_type_len) != 0)
	break;

      /* It's non-standard supervisor extension if it prefix with sx.  */
      if ((ext_type[0] == 's') && (ext_type_len == 1)
	  && (*(p + 1) == 'x'))
	break;

      char *subset = xstrdup (p);
      char *q = subset;
      const char *end_of_version;

      while (*++q != '\0' && *q != '_' && !ISDIGIT (*q))
	;

      end_of_version
	= parsing_subset_version (q, &major_version, &minor_version,
				  /* default_major_version= */ 2,
				  /* default_minor_version= */ 0,
				  /* std_ext_p= */ FALSE);

      *q = '\0';

      /* printf("balasr: subset=%s, major=%d, minor=%d\n", subset, major_version, minor_version); */
      /* make sure we fail when we encounter an unknown custom extension */
      if (!riscv_is_supported_pulp_ext(subset))
	{
	  warning_at (m_loc, 0, "%<-march=%s%>: %s is an unknown extension",
		    m_arch, subset);
	}

      add (subset, major_version, minor_version);
      free (subset);
      p += end_of_version - subset;

      if (*p != '\0' && *p != '_')
	{
	  error_at (m_loc, "%<-march=%s%>: %s must separate with _",
		    m_arch, ext_type_str);
	  return NULL;
	}
    }

  return p;
}

/* Parsing arch string to subset list, return NULL if parsing failed.  */

riscv_subset_list *
riscv_subset_list::parse (const char *arch, location_t loc)
{
  riscv_subset_list *subset_list = new riscv_subset_list (arch, loc);
  const char *p = arch;
  if (strncmp (p, "rv32", 4) == 0)
    {
      subset_list->m_xlen = 32;
      p += 4;
    }
  else if (strncmp (p, "rv64", 4) == 0)
    {
      subset_list->m_xlen = 64;
      p += 4;
    }
  else
    {
      error_at (loc, "%<-march=%s%>: ISA string must begin with rv32 or rv64",
		arch);
      goto fail;
    }

  /* Parsing standard extension.  */
  p = subset_list->parse_std_ext (p);

  if (p == NULL)
    goto fail;

  /* Parsing non-standard extension.  */
  p = subset_list->parse_sv_or_non_std_ext (p, "x", "non-standard extension");

  if (p == NULL)
    goto fail;

  /* Parsing supervisor extension.  */
  p = subset_list->parse_sv_or_non_std_ext (p, "s", "supervisor extension");

  if (p == NULL)
    goto fail;

  /* Parsing non-standard supervisor extension.  */
  p = subset_list->parse_sv_or_non_std_ext
    (p, "sx", "non-standard supervisor extension");

  if (p == NULL)
    goto fail;

  if (*p != '\0')
    {
      error_at (loc, "%<-march=%s%>: unexpected ISA string at end: %qs",
               arch, p);
      goto fail;
    }

  return subset_list;

fail:
  delete subset_list;
  return NULL;
}

/* Return the current arch string.  */

std::string
riscv_arch_str ()
{
  gcc_assert (current_subset_list);
  return current_subset_list->to_string ();
}

/* Parse a RISC-V ISA string into an option mask.  Must clear or set all arch
   dependent mask bits, in case more than one -march string is passed.  */

static void
riscv_parse_arch_string (const char *isa, int *flags, int *pulp_flags,
			 location_t loc)
{
  riscv_subset_list *subset_list;
  subset_list = riscv_subset_list::parse (isa, loc);
  if (!subset_list)
    return;

  if (subset_list->xlen () == 32)
    *flags &= ~MASK_64BIT;
  else if (subset_list->xlen () == 64)
    *flags |= MASK_64BIT;

  *flags &= ~MASK_RVE;
  if (subset_list->lookup ("e"))
    *flags |= MASK_RVE;

  *flags &= ~MASK_MUL;
  if (subset_list->lookup ("m"))
    *flags |= MASK_MUL;

  *flags &= ~MASK_ATOMIC;
  if (subset_list->lookup ("a"))
    *flags |= MASK_ATOMIC;

  *flags &= ~(MASK_HARD_FLOAT | MASK_DOUBLE_FLOAT);
  if (subset_list->lookup ("f"))
    *flags |= MASK_HARD_FLOAT;

  if (subset_list->lookup ("d"))
    *flags |= MASK_DOUBLE_FLOAT;

  *flags &= ~MASK_RVC;
  if (subset_list->lookup ("c"))
    *flags |= MASK_RVC;

  /* PULP specific extension parsing
     "none"
     "pulpv0"
     "pulpv1"
     "pulpv2
     "pulpv3"
     "gap8" */

  /* assume we don't need to run in pulpv0 pulpv1 compatibility mode. -march
     should override previous flags */
  *pulp_flags &= ~OPTION_MASK_PULP_COMPAT;

  *pulp_flags &= ~OPTION_MASK_PULP_HWLOOP;
  if (subset_list->lookup("xpulphwloop"))
    *pulp_flags |= OPTION_MASK_PULP_HWLOOP;

  *pulp_flags &= ~OPTION_MASK_PULP_POSTMOD;
  if (subset_list->lookup("xpulppostmod"))
    *pulp_flags |= OPTION_MASK_PULP_POSTMOD;
  /* xpulppostmodv0p0 is for pulpv0 pulpv1 */
  if (subset_list->lookup("xpulppostmod"), 0, 0)
    *pulp_flags |= OPTION_MASK_PULP_COMPAT;

  *pulp_flags &= ~OPTION_MASK_PULP_INDREGREG;
  if (subset_list->lookup("xpulpindregreg"))
    *pulp_flags |= OPTION_MASK_PULP_INDREGREG;

  *pulp_flags &= ~OPTION_MASK_PULP_MAC_SI;
  if (subset_list->lookup("xpulpmacsi"))
    *pulp_flags |= OPTION_MASK_PULP_MAC_SI;

  *pulp_flags &= ~OPTION_MASK_PULP_MULRN_HI;
  if (subset_list->lookup("xpulpmulrnhi"))
    *pulp_flags |= OPTION_MASK_PULP_MULRN_HI;

  *pulp_flags &= ~OPTION_MASK_PULP_MACRN_HI;
  if (subset_list->lookup("xpulpmacrnhi"))
    *pulp_flags |= OPTION_MASK_PULP_MACRN_HI;

  *pulp_flags &= ~OPTION_MASK_PULP_PARTMAC;
  if (subset_list->lookup("xpulppartmac"))
    *pulp_flags |= OPTION_MASK_PULP_PARTMAC;

  *pulp_flags &= ~OPTION_MASK_PULP_MAC_ALT;
  if (subset_list->lookup("xpulpmacalt"))
    *pulp_flags |= OPTION_MASK_PULP_MAC_ALT;

  *pulp_flags &= ~OPTION_MASK_PULP_MINMAX;
  if (subset_list->lookup("xpulpminmax"))
    *pulp_flags |= OPTION_MASK_PULP_MINMAX;
  /* xpulpminmaxv0p0 is for pulpv0 pulpv1 */
  if (subset_list->lookup("xpulpminmax"), 0, 0)
    *pulp_flags |= OPTION_MASK_PULP_COMPAT;

  *pulp_flags &= ~OPTION_MASK_PULP_ABS;
  if (subset_list->lookup("xpulpabs"))
    *pulp_flags |= OPTION_MASK_PULP_ABS;
  /* xpulpabsv0p0 is for pulpv0 pulpv1 */
  if (subset_list->lookup("xpulpabs"), 0, 0)
    *pulp_flags |= OPTION_MASK_PULP_COMPAT;

  *pulp_flags &= ~OPTION_MASK_PULP_BITOP;
  if (subset_list->lookup("xpulpbitop"))
    *pulp_flags |= OPTION_MASK_PULP_BITOP;

  *pulp_flags &= ~OPTION_MASK_PULP_BITOP_SMALL;
  if (subset_list->lookup("xpulpbitopsmall"))
    *pulp_flags |= OPTION_MASK_PULP_BITOP_SMALL;

  *pulp_flags &= ~OPTION_MASK_PULP_SLET;
  if (subset_list->lookup("xpulpslet"))
    *pulp_flags |= OPTION_MASK_PULP_SLET;

  *pulp_flags &= ~OPTION_MASK_PULP_VECT;
  if (subset_list->lookup("xpulpvect"))
    *pulp_flags |= OPTION_MASK_PULP_VECT;

  *pulp_flags &= ~OPTION_MASK_PULP_VECT_SHUFFLEPACK;
  if (subset_list->lookup("xpulpvectshufflepack"))
    *pulp_flags |= OPTION_MASK_PULP_VECT_SHUFFLEPACK;

  *pulp_flags &= ~OPTION_MASK_PULP_VECT_GAP8;
  if (subset_list->lookup("xpulpvectgap", 8, 0))
    *pulp_flags |= OPTION_MASK_PULP_VECT_GAP8;

  *pulp_flags &= ~OPTION_MASK_PULP_VECT;
  *pulp_flags &= ~OPTION_MASK_PULP_VECT_SHUFFLEPACK;
  if (subset_list->lookup("xpulpvectall"))
    {
      *pulp_flags |= OPTION_MASK_PULP_VECT;
      *pulp_flags |= OPTION_MASK_PULP_VECT_SHUFFLEPACK;
    }

  *pulp_flags &= ~OPTION_MASK_PULP_CLIP;
  if (subset_list->lookup("xpulpclip"))
    *pulp_flags |= OPTION_MASK_PULP_CLIP;

  *pulp_flags &= ~OPTION_MASK_PULP_ADDSUBRN;
  if (subset_list->lookup("xpulpaddsubrn"))
    *pulp_flags |= OPTION_MASK_PULP_ADDSUBRN;

  *pulp_flags &= ~OPTION_MASK_PULP_ELW;
  if (subset_list->lookup("xpulpelw"))
    *pulp_flags |= OPTION_MASK_PULP_ELW;

  /* groupings using the above listed subsets */
#define PULP_EXT_GROUP_SMALL (OPTION_MASK_PULP_POSTMOD		\
			      | OPTION_MASK_PULP_INDREGREG	\
			      | OPTION_MASK_PULP_BITOP_SMALL	\
			      | OPTION_MASK_PULP_MINMAX	\
			      | OPTION_MASK_PULP_SLET		\
			      | OPTION_MASK_PULP_MAC_ALT	\
			      | OPTION_MASK_PULP_ELW)

  /* pulpv0 (hwloops are disabled because buggy). Note we enable the backwards
     compatibility mode */
#define PULP_EXT_GROUP_V0 (PULP_EXT_GROUP_SMALL | OPTION_MASK_PULP_COMPAT)
  /* pulpv1 */
#define PULP_EXT_GROUP_V1  (PULP_EXT_GROUP_SMALL	\
			    | OPTION_MASK_PULP_HWLOOP	\
			    | OPTION_MASK_PULP_COMPAT)

#define PULP_EXP_GROUP_LARGE (OPTION_MASK_PULP_POSTMOD			\
			      | OPTION_MASK_PULP_INDREGREG		\
			      | OPTION_MASK_PULP_ABS			\
			      | OPTION_MASK_PULP_SLET			\
			      | OPTION_MASK_PULP_MINMAX			\
			      | OPTION_MASK_PULP_BITOP			\
			      | OPTION_MASK_PULP_CLIP			\
			      | OPTION_MASK_PULP_HWLOOP			\
			      | OPTION_MASK_PULP_MAC_SI			\
			      | OPTION_MASK_PULP_MACRN_HI		\
			      | OPTION_MASK_PULP_MULRN_HI		\
			      | OPTION_MASK_PULP_PARTMAC		\
			      | OPTION_MASK_PULP_ADDSUBRN		\
			      | OPTION_MASK_PULP_VECT			\
			      | OPTION_MASK_PULP_VECT_SHUFFLEPACK	\
			      | OPTION_MASK_PULP_BR			\
			      | OPTION_MASK_PULP_ELW)

  /* pulpv2 no difference to pulpv3 except for the mul extension hack that was
     turned into an erro*/
#define PULP_EXT_GROUP_V2 (PULP_EXP_GROUP_LARGE)
  /* pulpv3 */
#define PULP_EXT_GROUP_V3 (PULP_EXP_GROUP_LARGE)
  /* gap8 */
#define PULP_EXT_GROUP_GAP8 (PULP_EXP_GROUP_LARGE | OPTION_MASK_PULP_VECT_GAP8)

  if (subset_list->lookup("xpulpv"))
    {
      if (subset_list->lookup("xpulpv", 0, 0))
	{
	  warning_at(loc, 0, "%<-march=%s%>: pulpv0 is not supported well "
		     "anymore.", isa);

	  if (*flags & MASK_MUL)
	    warning_at(loc, 0, "%<-march=%s%>: m (multiplication) extension "
		       "is enabled with pulpv0. This was originally not "
		       "allowed", isa);

	  *pulp_flags &= ~PULP_EXT_GROUP_V0;

	  if (Pulp_Cpu == PULP_NONE || Pulp_Cpu == PULP_V0)
	    {
	      *pulp_flags |= PULP_EXT_GROUP_V0;

	      Pulp_Cpu = PULP_V0;
	    }
	  else
	    error("-xpulpv0: pulp architecture is already defined as %s",
		  PulpProcessorImage(Pulp_Cpu));
	}
      else if (subset_list->lookup("xpulpv", 1, 0))
	{
	  warning_at(loc, 0, "%<-march=%s%>: pulpv1 is not supported well "
		     "anymore.", isa);

	  if (*flags & MASK_MUL)
	    warning_at(loc, 0, "%<-march=%s%>: m (multiplication) extension "
		       "is enabled with pulpv1. This was originally not "
		       "allowed", isa);


	  *pulp_flags &= ~PULP_EXT_GROUP_V1;

	  if (Pulp_Cpu == PULP_NONE || Pulp_Cpu == PULP_V1)
	    {
	      *pulp_flags |= PULP_EXT_GROUP_V1;

	      Pulp_Cpu = PULP_V1;
	    }
	  else
	    error("-xpulpv1: pulp architecture is already defined as %s",
		  PulpProcessorImage(Pulp_Cpu));

	}
      else if (subset_list->lookup("xpulpv", 2, 0))
	{
	  /* we remove this mul blocking hack and turn it into a warning */
	  /* *flags &= ~MASK_MUL; */
	  if (*flags & MASK_MUL)
	    warning_at(loc, 0, "%<-march=%s%>: m (multiplication) extension "
		       "is enabled with pulpv2. This was originally not "
		       "allowed", isa);

	  *pulp_flags &= ~PULP_EXT_GROUP_V2;

	  if (Pulp_Cpu == PULP_NONE || Pulp_Cpu == PULP_V2)
	    {
	      /* TODO: make shufflepack imply vect or error */
	      *pulp_flags |= PULP_EXT_GROUP_V2;

	      Pulp_Cpu = PULP_V2;
	    }
	  else
	    error("-xpulpv2: pulp architecture is already defined as %s",
		  PulpProcessorImage(Pulp_Cpu));
	}
      else if (subset_list->lookup("xpulpv", 3, 0))
	{
	  /* *flags |= MASK_MUL; */
	  if (!(*flags & MASK_MUL))
	    warning_at(loc, 0, "%<-march=%s%>: m (multiplication) extension "
		       "is not enabled with pulpv3. This was originally not "
		       "allowed. Make sure you know what you do.", isa);

	  *pulp_flags &= ~PULP_EXT_GROUP_V3;

	  if (Pulp_Cpu == PULP_NONE || Pulp_Cpu == PULP_V3)
	    {
	      *pulp_flags |= PULP_EXT_GROUP_V3;

	      Pulp_Cpu = PULP_V3;
	    }
	  else
	    error("-xpulpv3: pulp architecture is already defined as %s",
		  PulpProcessorImage(Pulp_Cpu));
	  //printf("balasr: parsed xpulpv3\n");
	}
      else
	{
	  error_at (loc, "%<-march=%s%>: unknown xpulpv version",
		    isa);
	}

      if (*flags & MASK_64BIT)
	error_at (loc, "%<-march=%s%>: rv64 is not supported in this "
		  "configuration", isa);
    }

  if (subset_list->lookup("xgap", 8, 0))
    {
      /* *flags |= MASK_MUL; */
      if (!(*flags & MASK_MUL))
	warning_at(loc, 0, "%<-march=%s%>: m (multiplication) extension "
		   "is not enabled with gap8. This was originally not "
		   "allowed. Make sure you know what you do.", isa);

      *pulp_flags &= ~PULP_EXT_GROUP_GAP8;

      if (Pulp_Cpu == PULP_NONE || Pulp_Cpu == PULP_GAP8)
	{
	  *pulp_flags |= PULP_EXT_GROUP_GAP8;

	  Pulp_Cpu = PULP_GAP8;
	}
      else
	error("-xgap8: pulp architecture is already defined as %s",
	      PulpProcessorImage(Pulp_Cpu));

      if (*flags & MASK_64BIT)
	error_at (loc, "%<-march=%s%>: rv64 is not supported in this "
		  "configuration", isa);
    }


  if (subset_list->lookup("xpulpslim"))
    {

      if (Pulp_Cpu == PULP_NONE || Pulp_Cpu == PULP_GAP8)
	Pulp_Cpu = PULP_GAP8;
      else
	error("-xgap8: pulp architecture is already defined as %s",
	      PulpProcessorImage(Pulp_Cpu));

      if (Pulp_Cpu == PULP_NONE || Pulp_Cpu == PULP_SLIM)
	{
	  warning_at(loc, 0, "%<-march=%s%>: pulp_slim is not supported well "
		     "anymore.", isa);
	  Pulp_Cpu = PULP_SLIM;
	}
      else
	error("-xpulpslim: pulp architecture is already defined as %s",
	      PulpProcessorImage(Pulp_Cpu));

      if (*flags & MASK_64BIT)
	error_at (loc, "%<-march=%s%>: rv64 is not supported in this "
		  "configuration", isa);
    }

  /* TODO: PULP: disable indindreg forecefully since its buggy */
  *pulp_flags &= ~OPTION_MASK_PULP_INDREGREG;

  if (current_subset_list)
    delete current_subset_list;

  current_subset_list = subset_list;
}

/* Implement TARGET_HANDLE_OPTION.  */

static bool
riscv_handle_option (struct gcc_options *opts,
		     struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		     const struct cl_decoded_option *decoded,
		     location_t loc)
{
  bool defined = false;

  switch (decoded->opt_index)
    {
    case OPT_march_:
      riscv_parse_arch_string (decoded->arg, &opts->x_target_flags,
			       &opts->x_pulp_target_flags, loc);
      return true;

    /* pulp chip parsing */
    case OPT_mchip_:
      switch (decoded->value) {
      case PULP_CHIP_NONE:
	break;
      case PULP_CHIP_HONEY:
	riscv_parse_arch_string ("rv32ixpulpv0",  &opts->x_target_flags,
				 &opts->x_pulp_target_flags, loc);
	defined=true;
	break;
      case PULP_CHIP_PULPINO:
	riscv_parse_arch_string ("rv32ixpulpv1",  &opts->x_target_flags,
				 &opts->x_pulp_target_flags, loc);
	defined=true;
	break;
      case PULP_CHIP_GAP8:
	/* TODO: what is the correct arch string here? */
	riscv_parse_arch_string ("rv32imcxgap8",  &opts->x_target_flags,
				 &opts->x_pulp_target_flags, loc);
	defined=true;
	break;
      default:
	break;
      }
      if (defined) {
	/* TODO: remove global struct hack */
	_Pulp_FC = Pulp_Defined_Chips[decoded->value].Pulp_FC;
	_Pulp_PE = Pulp_Defined_Chips[decoded->value].Pulp_PE;
	_Pulp_L2_Size = Pulp_Defined_Chips[decoded->value].Pulp_L2_Size;
	_Pulp_L1_Cluster_Size = Pulp_Defined_Chips[decoded->value].Pulp_L1_Cluster_Size;
	_Pulp_L1_FC_Size = Pulp_Defined_Chips[decoded->value].Pulp_L1_FC_Size;
      }
      return true;

    case OPT_mcpu_:
      error("Use -march to pass pulp cpu info and not -mcpu");
      return true;

    default:
      return true;
    }
}

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options riscv_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fsection_anchors, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_free, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE riscv_option_optimization_table

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION riscv_handle_option

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
