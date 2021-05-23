cat > program.ts
tsc --lib DOM,ESNext --target ES2019 --strict \
    --skipLibCheck --types /usr/local/share/.config/yarn/global/node_modules/@types/node program.ts \
    && cat program.js | node
