---
title: News
summary: Recent updates, announcements, and media coverage.

# View options:
# 1 = List (standard linear listing)
# 2 = Compact (clean, compact list)
# 3 = Card (image grid cards)
# 4 = Citation (for academic papers)
view: 2

# Optional section header image (relative to `assets/media/` or `static/media/`)
header:
  caption: ""
  image: ""

# Page type settings
type: landing

# Options for layout blocks on this page
sections:
  - block: collection
    content:
      page_type: post
      count: 10
      filters:
        author: ""
        category: ""
        tag: ""
        exclude_featured: false
    design:
      columns: '1'
      view: 2
---
