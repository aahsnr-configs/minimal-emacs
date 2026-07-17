module.exports = function (eleventyConfig) {
  // 1. Passthrough Copy for Assets
  eleventyConfig.addPassthroughCopy({ "src/assets": "assets" });
  eleventyConfig.addWatchTarget("./src/assets/");

  // Enable raw HTML inside Markdown files (required for our complex accordion HTML)
  eleventyConfig.amendLibrary("md", (mdLib) => mdLib.enable("html"));

  return {
    dir: {
      input: "src",
      output: "_site",
      includes: "_includes",
      layouts: "_includes",
      data: "_data",
    },
    templateFormats: ["md", "njk", "html"],
    // CRITICAL FIX: Set to false to prevent Nunjucks from parsing Markdown content.
    // This stops Eleventy from choking on Elisp curly braces like `{{{` or `}}}`
    // inside code blocks. Markdown files are now treated as pure content, not templates.
    markdownTemplateEngine: false,
    htmlTemplateEngine: "njk",
    dataTemplateEngine: "njk",
  };
};
