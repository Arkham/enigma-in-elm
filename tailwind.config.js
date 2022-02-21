module.exports = {
  content: [
    './src/Main.elm',
    './src/index.html'
  ],
  theme: {
    // override focus ring color
    extend: {
      colors: {
        blue: {
          600: 'rgb(203 213 225)'
        },
        gray: {
          500: 'rgb(229 231 235)'
        }
      }
    }
  },
  plugins: [
    require('@tailwindcss/forms')
  ]
}
